{-|
Module      : Text.ReadP
Description : ReadP parsing
Maintainer  : David Baynard <davidbaynard@gmail.com>

License     : Apache

This implements (almost all) of Edward Kmett’s parsers interface <https://hackage.haskell.org/package/parsers> for 'ReadP'.

-}

{-# LANGUAGE ExistentialQuantification #-}

module Text.ReadP (
    ReadP
  , TR.Read
  , TR.ReadPrec
  , TR.readPrec
  , TR.readMaybe
  , TR.readEither
  , module Text.ReadP
)   where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.String (IsString, fromString)
import Data.List (transpose)
import Numeric

import qualified Text.Read as TR
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import qualified Text.ParserCombinators.ReadPrec as RP

import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import Data.CharSet (CharSet(..))
import qualified Data.CharSet as CharSet
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci

type Text = T.Text

{-|
  Run a parser on input
-}
parse :: ReadP a -> String -> [(a, String)]
parse = R.readP_to_S
infixl 2 `parse`

{-|
  Use a 'ReadP' parser in the 'Read' typeclass.
-}
makeRead :: ReadP a -> RP.ReadPrec a
makeRead = RP.lift

--------------------------------------------------
-- * Parsing
--------------------------------------------------

-- | Take a parser that may consume input, and on failure, go back to
-- where we started and fail as if we didn't consume input.
try :: ReadP a -> ReadP a
try = id

-- | Give a parser a name
(<?>) :: ReadP a -> String -> ReadP a
(<?>) = const

-- | A version of many that discards its input. Specialized because it
-- can often be implemented more cheaply.
skipMany :: ReadP a -> ReadP ()
skipMany = R.skipMany
{-# INLINE skipMany #-}

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
-- its result. (aka skipMany1 in parsec)
skipSome :: ReadP a -> ReadP ()
skipSome = R.skipMany1
{-# INLINE skipSome #-}

-- | Used to emit an error on an unexpected token
unexpected :: String -> ReadP a
unexpected = const R.pfail
{-# INLINE unexpected #-}

-- | This parser only succeeds at the end of the input. This is not a
-- primitive parser but it is defined using 'notFollowedBy'.
--
-- >  eof  = notFollowedBy anyChar <?> "end of input"
eof :: ReadP ()
eof = R.eof
{-# INLINE eof #-}

-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
-- does not consume any input. This parser can be used to implement the
-- \'longest match\' rule. For example, when recognizing keywords (for
-- example @let@), we want to make sure that a keyword is not followed
-- by a legal identifier character, in which case the keyword is
-- actually an identifier (for example @lets@). We can program this
-- behaviour as follows:
--
-- >  keywordLet  = try $ string "let" <* notFollowedBy alphaNum
notFollowedBy :: Show a => ReadP a -> ReadP ()
notFollowedBy p = ((Just <$> p) R.<++ pure Nothing)
    >>= maybe (pure ()) (unexpected . show)

--------------------------------------------------
-- ** Derived parsers
--------------------------------------------------

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.
choice :: Alternative m => [m a] -> m a
choice = asum
{-# INLINE choice #-}

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- >  priority = option 0 (digitToInt <$> digit)
option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x
{-# INLINE option #-}

-- | @skipOptional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. It discards the result
-- of @p@. (Plays the role of parsec's optional, which conflicts with Applicative's optional)
skipOptional :: Alternative m => m a -> m ()
skipOptional p = (() <$ p) <|> pure ()
{-# INLINE skipOptional #-}

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- >  braces  = between (symbol "{") (symbol "}")
between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket
{-# INLINE between #-}

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
{-# INLINE sepBy1 #-}

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.
sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = p <**> ((flip (:) <$> (sep *> sepEndBy p sep)) <|> pure pure)

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []
{-# INLINE sepEndBy #-}

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
endBy1 :: Alternative m => m a -> m sep -> m [a]
endBy1 p sep = some (p <* sep)
{-# INLINE endBy1 #-}

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi
endBy :: Alternative m => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.
count :: Applicative m => Int -> m a -> m [a]
count n p | n <= 0    = pure []
          | otherwise = sequenceA (replicate n p)
{-# INLINE count #-}

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.
chainr :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = chainr1 p op <|> pure x
{-# INLINE chainr #-}

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.
chainl :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x
{-# INLINE chainl #-}

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. . This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr   = term   `chainl1` addop
-- >  term   = factor `chainl1` mulop
-- >  factor = parens expr <|> integer
-- >
-- >  mulop  = (*) <$ symbol "*"
-- >       <|> div <$ symbol "/"
-- >
-- >  addop  = (+) <$ symbol "+"
-- >       <|> (-) <$ symbol "-"
chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan where
  scan = p <**> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

-- | @chainr1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan where
  scan = p <**> rst
  rst = (flip <$> op <*> scan) <|> pure id
{-# INLINE chainr1 #-}

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = do{ string "<!--"
-- >                      ; manyTill anyChar (try (string "-->"))
-- >                      }
--
--    Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
--    therefore the use of the 'try' combinator.
manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)
{-# INLINE manyTill #-}

infixr 0 <?>

--------------------------------------------------
-- * Character parsing
--------------------------------------------------
-- Additional functionality needed to parse character streams.

-- | Parse a single character of the input, with UTF-8 decoding
satisfy :: (Char -> Bool) -> ReadP Char
satisfy = R.satisfy
{-# INLINE satisfy #-}

-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- /e.g./
--
-- @semiColon = 'char' ';'@
char :: Char -> ReadP Char
char = R.char
{-# INLINE char #-}

-- | @notChar c@ parses any single character other than @c@. Returns the parsed
-- character.
notChar :: Char -> ReadP Char
notChar c = R.satisfy (c /=)
{-# INLINE notChar #-}

-- | This parser succeeds for any character. Returns the parsed character.
anyChar :: ReadP Char
anyChar = R.get
{-# INLINE anyChar #-}

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div"
-- >              <|> string "mod"
string :: String -> ReadP String
string = R.string
{-# INLINE string #-}

-- | @text t@ parses a sequence of characters determined by the text @t@ Returns
-- the parsed text fragment (i.e. @t@).
--
-- Using @OverloadedStrings@:
--
-- >  divOrMod    =   text "div"
-- >              <|> text "mod"
text :: Text -> ReadP Text
text t = t <$ string (T.unpack t)
{-# INLINE text #-}

--------------------------------------------------
-- ** Derived
--------------------------------------------------

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOf :: [Char] -> ReadP Char
oneOf xs = oneOfSet (CharSet.fromList xs)
{-# INLINE oneOf #-}
{-# ANN oneOf "HLint: ignore Use String" #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: [Char] -> ReadP Char
noneOf xs = noneOfSet (CharSet.fromList xs)
{-# INLINE noneOf #-}
{-# ANN noneOf "HLint: ignore Use String" #-}

-- | @oneOfSet cs@ succeeds if the current character is in the supplied
-- set of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOfSet :: CharSet -> ReadP Char
oneOfSet (CharSet True _ is)  = satisfy (\c -> IntSet.member (fromEnum c) is)
oneOfSet (CharSet False _ is) = satisfy (\c -> not (IntSet.member (fromEnum c) is))
{-# INLINE oneOfSet #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOfSet :: CharSet -> ReadP Char
noneOfSet s = oneOfSet (CharSet.complement s)
{-# INLINE noneOfSet #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
spaces :: ReadP ()
spaces = skipMany space <?> "white space"
{-# INLINE spaces #-}

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.
space :: ReadP Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Parses a newline character (\'\\n\'). Returns a newline character.
newline :: ReadP Char
newline = char '\n' <?> "new-line"
{-# INLINE newline #-}

-- | Parses a tab character (\'\\t\'). Returns a tab character.
tab :: ReadP Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses an upper case letter. Returns the parsed character.
upper :: ReadP Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character. Returns the parsed character.
lower :: ReadP Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit. Returns the parsed character.
alphaNum :: ReadP Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
{-# INLINE alphaNum #-}

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character.
letter :: ReadP Char
letter = satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parses a digit. Returns the parsed character.
digit :: ReadP Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.
hexDigit :: ReadP Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigit #-}

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.
octDigit :: ReadP Char
octDigit = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigit #-}

satisfyRange :: Char -> Char -> ReadP Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)
{-# INLINE satisfyRange #-}

--------------------------------------------------
-- * Lookahead
--------------------------------------------------
-- Additional functionality needed to describe parsers independent of input type.

-- | @lookAhead p@ parses @p@ without consuming any input.
lookAhead :: ReadP a -> ReadP a
lookAhead p = R.look >>= \s ->
                R.choice $ map (return . fst) $ R.readP_to_S p s

--------------------------------------------------
-- * Token parsing
--------------------------------------------------
-- Additional functionality that is needed to tokenize input while ignoring whitespace.

-- | Usually, someSpace consists of /one/ or more occurrences of a 'space'.
-- Some parsers may choose to recognize line comments or block (multi line)
-- comments as white space as well.
someSpace :: ReadP ()
someSpace = skipSome (satisfy isSpace)
{-# INLINE someSpace #-}

-- | Called when we enter a nested pair of symbols.
-- Overloadable to enable disabling layout
nesting :: ReadP a -> ReadP a
nesting = id
{-# INLINE nesting #-}

-- | The token parser |semi| parses the character \';\' and skips
-- any trailing white space. Returns the character \';\'. Overloadable to
-- permit automatic semicolon insertion or Haskell-style layout.
semi :: ReadP Char
semi = token (satisfy (';' ==) <?> ";")
{-# INLINE semi #-}

-- | @token p@ first applies parser @p@ and then the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (token) is defined using @token@, this way every parse
-- starts at a point without white space. Parsers that use @token@ are
-- called /token/ parsers in this document.
--
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- Alternatively, one might define 'token' as first parsing 'whiteSpace'
-- and then parser @p@.  By parsing whiteSpace first, the parser is able
-- to return before parsing additional whiteSpace, improving laziness.
--
-- > mainParser  = sum <$ whiteSpace <*> many (token digit) <* eof
token :: ReadP a -> ReadP a
token p = p <* (someSpace <|> pure ())

--------------------------------------------------
-- * Derived
--------------------------------------------------

-- | Skip zero or more bytes worth of white space. More complex parsers are
-- free to consider comments as white space.
whiteSpace :: ReadP ()
whiteSpace = someSpace <|> pure ()
{-# INLINE whiteSpace #-}

-- | This token parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
charLiteral :: ReadP Char
charLiteral = token lit where
  lit = between (char '\'') (char '\'' <?> "end of character") characterChar
    <?> "character"
{-# INLINE charLiteral #-}

-- | This token parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
stringLiteral :: IsString s => ReadP s
stringLiteral = fromString <$> token lit where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral #-}

-- | This token parser behaves as 'stringLiteral', but for single-quoted
-- strings.
stringLiteral' :: IsString s => ReadP s
stringLiteral' = fromString <$> token lit where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '\'') (char '\'' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

  stringEscape = char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral' #-}

-- | This token parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
natural :: ReadP Integer
natural = token natural'
{-# INLINE natural #-}

-- | This token parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
integer :: ReadP Integer
integer = token (token (sgn <*> natural')) <?> "integer"
  where
  sgn = negate <$ char '-'
    <|> id <$ char '+'
    <|> pure id
{-# INLINE integer #-}

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
double :: ReadP Double
double = token (Sci.toRealFloat <$> floating <?> "double")
{-# INLINE double #-}

-- | This token parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrDouble :: ReadP (Either Integer Double)
naturalOrDouble = fmap Sci.toRealFloat <$> naturalOrScientific
{-# INLINE naturalOrDouble #-}

-- | This token parser is like 'naturalOrDouble', but handles
-- leading @-@ or @+@.
integerOrDouble :: ReadP (Either Integer Double)
integerOrDouble = fmap Sci.toRealFloat <$> integerOrScientific
{-# INLINE integerOrDouble #-}

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
scientific :: ReadP Scientific
scientific = token (floating <?> "scientific")
{-# INLINE scientific #-}

-- | This token parser parses either 'natural' or a 'scientific'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrScientific :: ReadP (Either Integer Scientific)
naturalOrScientific = token (natFloating <?> "number")
{-# INLINE naturalOrScientific #-}

-- | This token parser is like 'naturalOrScientific', but handles
-- leading @-@ or @+@.
integerOrScientific :: ReadP (Either Integer Scientific)
integerOrScientific = token (ios <?> "number")
  where ios = mneg <$> optional (oneOf "+-") <*> natFloating
        mneg (Just '-') nd = either (Left . negate) (Right . negate) nd
        mneg _          nd = nd
{-# INLINE integerOrScientific #-}


-- | Token parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space.
symbol :: String -> ReadP String
symbol name = token (string name)
{-# INLINE symbol #-}

-- | Token parser @textSymbol t@ parses 'text' @s@ and skips
-- trailing white space.
textSymbol :: Text -> ReadP Text
textSymbol name = token (text name)
{-# INLINE textSymbol #-}

-- | Token parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space.
symbolic :: Char -> ReadP Char
symbolic name = token (char name)
{-# INLINE symbolic #-}

-- | Token parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parens :: ReadP a -> ReadP a
parens = nesting . between (symbolic '(') (symbolic ')')
{-# INLINE parens #-}

-- | Token parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@.
braces :: ReadP a -> ReadP a
braces = nesting . between (symbolic '{') (symbolic '}')
{-# INLINE braces #-}

-- | Token parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@.
angles :: ReadP a -> ReadP a
angles = nesting . between (symbolic '<') (symbolic '>')
{-# INLINE angles #-}

-- | Token parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
brackets :: ReadP a -> ReadP a
brackets = nesting . between (symbolic '[') (symbolic ']')
{-# INLINE brackets #-}

-- | Token parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\".
comma :: ReadP Char
comma = symbolic ','
{-# INLINE comma #-}

-- | Token parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\".
colon :: ReadP Char
colon = symbolic ':'
{-# INLINE colon #-}

-- | Token parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\".
dot :: ReadP Char
dot = symbolic '.'
{-# INLINE dot #-}

-- | Token parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep :: ReadP a -> ReadP [a]
semiSep p = sepBy p semi
{-# INLINE semiSep #-}

-- | Token parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep1 :: ReadP a -> ReadP [a]
semiSep1 p = sepBy1 p semi
{-# INLINE semiSep1 #-}

-- | Token parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep :: ReadP a -> ReadP [a]
commaSep p = sepBy p comma
{-# INLINE commaSep #-}

-- | Token parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep1 :: ReadP a -> ReadP [a]
commaSep1 p = sepBy1 p comma
{-# INLINE commaSep1 #-}

--------------------------------------------------
-- ** Utilities
--------------------------------------------------

-- | This parser parses a character literal without the surrounding quotation marks.
--
-- This parser does NOT swallow trailing whitespace

characterChar :: ReadP Char
characterChar = charLetter <|> charEscape <?> "literal character"
{-# INLINE characterChar #-}

charEscape:: ReadP Char
charEscape = char '\\' *> escapeCode
{-# INLINE charEscape #-}

charLetter :: ReadP Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
{-# INLINE charLetter #-}

-- | This parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- This parser does NOT swallow trailing whitespace
escapeCode :: ReadP Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))
  charNum = toEnum <$> num
    where
      num = bounded 10 maxchar
        <|> (char 'o' *> bounded 8 maxchar)
        <|> (char 'x' *> bounded 16 maxchar)
      maxchar = fromEnum (maxBound :: Char)

  bounded base bnd = foldl' (\x d -> base * x + digitToInt d) 0
                 <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
    where
      thedigits = map char ['0'..'9'] ++ map oneOf (transpose [['A'..'F'],['a'..'f']])
      toomuch = unexpected "out-of-range numeric escape sequence"
      bounded' dps@(zero:_) bds = skipSome zero *> ([] <$ notFollowedBy (choice dps) <|> bounded'' dps bds)
                              <|> bounded'' dps bds
      bounded' []           _   = error "bounded called with base 0"
      bounded'' dps []         = [] <$ notFollowedBy (choice dps) <|> toomuch
      bounded'' dps (bd : bds) = let anyd = choice dps
                                     nomore = notFollowedBy anyd <|> toomuch
                                     (low, ex : high) = splitAt bd dps
                                  in ((:) <$> choice low <*> atMost (length bds) anyd) <* nomore
                                     <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                                     <|> if not (null bds)
                                            then (:) <$> choice high <*> atMost (length bds - 1) anyd <* nomore
                                            else empty
      atMost n p | n <= 0    = pure []
                 | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

-- | This parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
natural' :: ReadP Integer
natural' = nat <?> "natural"

number :: Integer -> ReadP Char -> ReadP Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

-- | This parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
--
-- Also, unlike the 'integer' parser, this parser does not admit spaces
-- between the sign and the number.
integer' :: ReadP Integer
integer' = int <?> "integer"
{-# INLINE integer' #-}

sign :: ReadP (Integer -> Integer)
sign = negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

int :: ReadP Integer
int = {-token-} sign <*> nat

nat :: ReadP Integer
nat = zeroNumber <|> decimal

zeroNumber :: ReadP Integer
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> pure 0) <?> ""

floating :: ReadP Scientific
floating = decimal <**> fractExponent
{-# INLINE floating #-}

fractExponent :: ReadP (Integer -> Scientific)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent'
 where
  fraction = foldl' op 0 <$> (char '.' *> (some digit <?> "fraction"))
  op f d = f + Sci.scientific (fromIntegral (digitToInt d)) (Sci.base10Exponent f - 1)
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"
  power = Sci.scientific 1 . fromInteger


natFloating, zeroNumFloat, decimalFloat :: ReadP (Either Integer Scientific)
natFloating
    = char '0' *> zeroNumFloat
  <|> decimalFloat
zeroNumFloat
    = Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> pure 0 <**> try fractFloat
  <|> pure (Left 0)
decimalFloat = decimal <**> option Left (try fractFloat)

fractFloat :: ReadP (Integer -> Either Integer Scientific)
fractFloat = (Right .) <$> fractExponent
{-# INLINE fractFloat #-}

-- | Parses a positive whole number in the decimal system. Returns the
-- value of the number.
--
-- This parser does NOT swallow trailing whitespace
decimal :: ReadP Integer
decimal = number 10 digit
{-# INLINE decimal #-}

-- | Parses a positive whole number in the hexadecimal system. The number
-- should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
hexadecimal :: ReadP Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit
{-# INLINE hexadecimal #-}

-- | Parses a positive whole number in the octal system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
octal :: ReadP Integer
octal = oneOf "oO" *> number 8 octDigit
{-# INLINE octal #-}

----------------------------------------------------------------
--  Building a permutation parser
----------------------------------------------------------------

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@.
(<||>) :: Functor m => Permutation m (a -> b) -> m a -> Permutation m b
(<||>) = add
{-# INLINE (<||>) #-}

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is not allowed to accept empty input - use the optional
-- combinator ('<$?>') instead.
--
-- If the function @f@ takes more than one parameter, the type variable
-- @b@ is instantiated to a functional type which combines nicely with
-- the adds parser @p@ to the ('<||>') combinator. This
-- results in stylized code where a permutation parser starts with a
-- combining function @f@ followed by the parsers. The function @f@
-- gets its parameters in the order in which the parsers are specified,
-- but actual input can be in any order.
(<$$>) :: Functor m => (a -> b) -> m a -> Permutation m b
(<$$>) f p = newPermutation f <||> p
{-# INLINE (<$$>) #-}

-- | The expression @perm \<||> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@.
(<|?>) :: Functor m => Permutation m (a -> b) -> (a, m a) -> Permutation m b
(<|?>) perm (x,p) = addOpt perm x p
{-# INLINE (<|?>) #-}

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead.

(<$?>) :: Functor m => (a -> b) -> (a, m a) -> Permutation m b
(<$?>) f (x,p) = newPermutation f <|?> (x,p)
{-# INLINE (<$?>) #-}

----------------------------------------------------------------
-- * The permutation tree
----------------------------------------------------------------

-- | The type @Permutation m a@ denotes a permutation parser that,
-- when converted by the 'permute' function, parses
-- using the base parsing monad @m@ and returns a value of
-- type @a@ on success.
--
-- Normally, a permutation parser is first build with special operators
-- like ('<||>') and than transformed into a normal parser
-- using 'permute'.
data Permutation m a = Permutation (Maybe a) [Branch m a]

instance Functor m => Functor (Permutation m) where
  fmap f (Permutation x xs) = Permutation (fmap f x) (fmap f <$> xs)

data Branch m a = forall b. Branch (Permutation m (b -> a)) (m b)

instance Functor m => Functor (Branch m) where
  fmap f (Branch perm p) = Branch (fmap (f.) perm) p

-- | The parser @permute perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of:
-- an optional string of @a@'s, the character @b@ and an optional @c@.
-- This can be described by:
--
-- >  test  = permute (tuple <$?> ("",some (char 'a'))
-- >                         <||> char 'b'
-- >                         <|?> ('_',char 'c'))
-- >        where
-- >          tuple a b c  = (a,b,c)
-- transform a permutation tree into a normal parser
permute :: Alternative m => Permutation m a -> m a
permute (Permutation def xs)
  = asum (map branch xs ++ e)
  where
    e = maybe [] (pure . pure) def
    branch (Branch perm p) = flip id <$> p <*> permute perm

-- build permutation trees
newPermutation :: (a -> b) -> Permutation m (a -> b)
newPermutation f = Permutation (Just f) []
{-# INLINE newPermutation #-}

add :: Functor m => Permutation m (a -> b) -> m a -> Permutation m b
add perm@(Permutation _mf fs) p
  = Permutation Nothing (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p')
            = Branch (add (fmap flip perm') p) p'

addOpt :: Functor m => Permutation m (a -> b) -> a -> m a -> Permutation m b
addOpt perm@(Permutation mf fs) x p
  = Permutation (fmap ($ x) mf) (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p') = Branch (addOpt (fmap flip perm') x p) p'
