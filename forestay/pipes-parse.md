# Pipes.Prelude

``` {haskell}
all :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
and :: Monad m => Producer Bool m () -> m Bool
any :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
chain :: Monad m => (a -> m ()) -> Pipe a a m r
concat :: (Monad m, Foldable f) => Pipe (f a) a m r
drain :: Monad m => Consumer' a m r
drop :: Monad m => Int -> Pipe a a m r
dropWhile :: Monad m => (a -> Bool) -> Pipe a a m r
elem :: (Monad m, Eq a) => a -> Producer a m () -> m Bool
elemIndices :: (Monad m, Eq a) => a -> Pipe a Int m r
filter :: Monad m => (a -> Bool) -> Pipe a a m r
filterM :: Monad m => (a -> m Bool) -> Pipe a a m r
find :: Monad m => (a -> Bool) -> Producer a m () -> m (Maybe a)
findIndex :: Monad m => (a -> Bool) -> Producer a m () -> m (Maybe Int)
findIndices :: Monad m => (a -> Bool) -> Pipe a Int m r
fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
fold' :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Producer a m r -> m (b, r)
foldM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m () -> m b
foldM' :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m r -> m (b, r)
fromHandle :: MonadIO m => IO.Handle -> Producer' String m ()
generalize :: Monad m => Pipe a b m r -> x -> Proxy x a x b m r
head :: Monad m => Producer a m () -> m (Maybe a)
index :: Monad m => Int -> Producer a m () -> m (Maybe a)
last :: Monad m => Producer a m () -> m (Maybe a)
length :: Monad m => Producer a m () -> m Int
loop :: Monad m => (a -> ListT m b) -> Pipe a b m r
map :: Monad m => (a -> b) -> Pipe a b m r
mapFoldable :: (Monad m, Foldable t) => (a -> t b) -> Pipe a b m r
mapM :: Monad m => (a -> m b) -> Pipe a b m r
mapM_ :: Monad m => (a -> m ()) -> Consumer' a m r
maximum :: (Monad m, Ord a) => Producer a m () -> m (Maybe a)
minimum :: (Monad m, Ord a) => Producer a m () -> m (Maybe a)
notElem :: (Monad m, Eq a) => a -> Producer a m () -> m Bool
null :: Monad m => Producer a m () -> m Bool
or :: Monad m => Producer Bool m () -> m Bool
print :: (MonadIO m, Show a) => Consumer' a m r
product :: (Monad m, Num a) => Producer a m () -> m a
read :: (Monad m, Read a) => Pipe String a m r
readLn :: (MonadIO m, Read a) => Producer' a m ()
repeatM :: Monad m => m a -> Producer' a m r
replicateM :: Monad m => Int -> m a -> Producer' a m ()
scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Pipe a b m r
scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Pipe a b m r
seq :: Monad m => Pipe a a m r
sequence :: Monad m => Pipe (m a) a m r
show :: (Monad m, Show a) => Pipe a String m r
stdinLn :: MonadIO m => Producer' String m ()
stdoutLn :: MonadIO m => Consumer' String m ()
stdoutLn' :: MonadIO m => Consumer' String m r
sum :: (Monad m, Num a) => Producer a m () -> m a
take :: Monad m => Int -> Pipe a a m ()
takeWhile :: Monad m => (a -> Bool) -> Pipe a a m ()
takeWhile' :: Monad m => (a -> Bool) -> Pipe a a m a
tee :: Monad m => Consumer a m r -> Pipe a a m r
toHandle :: MonadIO m => IO.Handle -> Consumer' String m r
toList :: Producer a Identity () -> [a]
toListM :: Monad m => Producer a m () -> m [a]
toListM' :: Monad m => Producer a m r -> m ([a], r)
unfoldr :: Monad m 
zip :: Monad m => (Producer   a     m r) -> (Producer      b  m r) -> (Producer' (a, b) m r)
```

# Pipes.ByteString

``` {haskell}
(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
_lines :: Monad m => Producer ByteString m x -> FreeT (Producer ByteString m) m x
_pack :: Monad m => Producer Word8 m x -> Producer ByteString m x
_unlines :: Monad m => FreeT (Producer ByteString m) m x -> Producer ByteString m x
_unpack :: Monad m => Producer ByteString m x -> Producer Word8 m x
all :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m Bool
any :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m Bool
break :: Monad m => (Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
chunksOf :: (Monad m, Integral n) => n -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
chunksOf' :: (Monad m, Integral n) => n -> Producer ByteString m r -> Producer ByteString m r
concatMap :: Monad m => (Word8 -> ByteString) -> Pipe ByteString ByteString m r
count :: (Monad m, Num n) => Word8 -> Producer ByteString m () -> m n
drawByte :: Monad m => Parser ByteString m (Maybe Word8)
drop :: (Monad m, Integral n) => n -> Producer ByteString m r -> Producer ByteString m r
dropWhile :: Monad m => (Word8 -> Bool) -> Producer ByteString m r -> Producer ByteString m r
elem :: Monad m => Word8 -> Producer ByteString m () -> m Bool
elemIndex :: (Monad m, Num n) => Word8 -> Producer ByteString m () -> m (Maybe n)
elemIndices :: (Monad m, Num n) => Word8 -> Pipe ByteString n m r
filter :: Monad m => (Word8 -> Bool) -> Pipe ByteString ByteString m r
find :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m (Maybe Word8)
findIndex :: (Monad m, Num n) => (Word8 -> Bool) -> Producer ByteString m () -> m (Maybe n)
findIndices :: (Monad m, Num n) => (Word8 -> Bool) -> Pipe ByteString n m r
foldBytes :: Monad m => (x -> Word8 -> x) -> x -> (x -> r) -> Producer ByteString m () -> m r
fromHandle :: MonadIO m => IO.Handle -> Producer' ByteString m ()
fromLazy :: Monad m => BL.ByteString -> Producer' ByteString m ()
group :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
groupBy :: Monad m => (Word8 -> Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
groups :: Monad m => Producer ByteString m x :~> FreeT (Producer ByteString m) m x
groupsBy :: Monad m => (Word8 -> Word8 -> Bool) -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
hGet :: MonadIO m => Int -> IO.Handle -> Producer' ByteString m ()
hGetN :: MonadIO m => IO.Handle -> Int -> Server' Int ByteString m ()
hGetNonBlocking :: MonadIO m => Int -> IO.Handle -> Producer' ByteString m ()
hGetRange :: MonadIO m => Int -- ^ Offset -> Int -- ^ Size -> IO.Handle -> Producer' ByteString m ()
hGetSome :: MonadIO m => Int -> IO.Handle -> Producer' ByteString m ()
hGetSomeN :: MonadIO m => IO.Handle -> Int -> Server' Int ByteString m ()
head :: Monad m => Producer ByteString m () -> m (Maybe Word8)
index :: (Monad m, Integral n) => n -> Producer ByteString m () -> m (Maybe Word8)
intersperse :: Monad m => Word8 -> Producer ByteString m r -> Producer ByteString m r
isEndOfBytes :: Monad m => Parser ByteString m Bool
last :: Monad m => Producer ByteString m () -> m (Maybe Word8)
length :: (Monad m, Num n) => Producer ByteString m () -> m n
line :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
lines :: Monad m => Producer ByteString m x :~> FreeT (Producer ByteString m) m x
map :: Monad m => (Word8 -> Word8) -> Pipe ByteString ByteString m r
maximum :: Monad m => Producer ByteString m () -> m (Maybe Word8)
minimum :: Monad m => Producer ByteString m () -> m (Maybe Word8)
nextByte :: Monad m => Producer ByteString m r -> m (Either r (Word8, Producer ByteString m r))
nl :: Word8
notElem :: Monad m => Word8 -> Producer ByteString m () -> m Bool
null :: Monad m => Producer ByteString m () -> m Bool
pack :: Monad m => Producer Word8 m x :~> Producer ByteString m x
peekByte :: Monad m => Parser ByteString m (Maybe Word8)
scan :: Monad m => (Word8 -> Word8 -> Word8) -> Word8 -> Pipe ByteString ByteString m r
span :: Monad m => (Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
splitAt :: (Monad m, Integral n) => n -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
splits :: Monad m => Word8 -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
splitsWith :: Monad m => (Word8 -> Bool) -> Producer ByteString m x -> FreeT (Producer ByteString m) m x
stdin :: MonadIO m => Producer' ByteString m ()
stdout :: MonadIO m => Consumer' ByteString m ()
take :: (Monad m, Integral n) => n -> Pipe ByteString ByteString m ()
takeWhile :: Monad m => (Word8 -> Bool) -> Pipe ByteString ByteString m ()
toHandle :: MonadIO m => IO.Handle -> Consumer' ByteString m r
toLazy :: Producer ByteString Identity () -> BL.ByteString
toLazyM :: Monad m => Producer ByteString m () -> m BL.ByteString
unDrawByte :: Monad m => Word8 -> Parser ByteString m ()
unlines :: Monad m => FreeT (Producer ByteString m) m x :~> Producer ByteString m x
unpack :: Monad m => Producer ByteString m x :~> Producer Word8 m x
unwords :: Monad m => FreeT (Producer ByteString m) m x -> Producer ByteString m x
word :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
words :: Monad m => Producer ByteString m x -> FreeT (Producer ByteString m) m x
```

# Pipes.Text

``` {haskell}
(^.) :: a -> ((b -> Constant b b) -> (a -> Constant b a)) -> b
_lines :: Monad m => Producer Text m r -> FreeT (Producer Text m) m r
_pack :: Monad m => Producer Char m r -> Producer Text m r
_unlines :: Monad m => FreeT (Producer Text m) m r -> Producer Text m r
_unpack :: Monad m => Producer Text m r -> Producer Char m r
_unwords :: (Monad m) => FreeT (Producer Text m) m r -> Producer Text m r
_words :: (Monad m) => Producer Text m r -> FreeT (Producer Text m) m r
all :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
any :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
break :: (Monad m) => (Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
chunksOf :: (Monad m, Integral n) => n -> Producer Text m r :~> FreeT (Producer Text m) m r
concatMap :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
defaultChunkSize :: Int
drawChar :: (Monad m) => Parser Text m (Maybe Char)
drop :: (Monad m, Integral n) => n -> Producer Text m r -> Producer Text m r
dropWhile :: (Monad m) => (Char -> Bool) -> Producer Text m r -> Producer Text m r
filter :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
find :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m (Maybe Char)
foldChars :: Monad m => (x -> Char -> x) -> x -> (x -> r) -> Producer Text m () -> m r
fromLazy :: (Monad m) => TL.Text -> Producer' Text m ()
group :: Monad m => Producer Text m r :~> Producer Text m (Producer Text m r)
groupBy :: (Monad m) => (Char -> Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
groups :: Monad m => Producer Text m x :~> FreeT (Producer Text m) m x
groupsBy :: Monad m => (Char -> Char -> Bool) -> Producer Text m x :~> FreeT (Producer Text m) m x
head :: (Monad m) => Producer Text m () -> m (Maybe Char)
index :: (Monad m, Integral a) => a-> Producer Text m () -> m (Maybe Char)
intercalate :: (Monad m) => Producer Text m () -> FreeT (Producer Text m) m r -> Producer Text m r
intersperse :: (Monad m) => Char -> Producer Text m r -> Producer Text m r
isEndOfChars :: (Monad m) => Parser Text m Bool
last :: (Monad m) => Producer Text m () -> m (Maybe Char)
length :: (Monad m, Num n) => Producer Text m () -> m n
line :: (Monad m) => Producer Text m r :~> Producer Text m (Producer Text m r)
lines :: (Monad m) => Producer Text m r :~>  FreeT (Producer Text m) m r
map :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
maximum :: (Monad m) => Producer Text m () -> m (Maybe Char)
minimum :: (Monad m) => Producer Text m () -> m (Maybe Char)
nextChar :: (Monad m) => Producer Text m r -> m (Either r (Char, Producer Text m r))
null :: (Monad m) => Producer Text m () -> m Bool
pack :: Monad m => Producer Char m r :~> Producer Text m r
peekChar :: (Monad m) => Parser Text m (Maybe Char)
scan :: (Monad m) => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
span :: (Monad m) => (Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
splitAt :: (Monad m, Integral n) => n -> Producer Text m r :~> Producer Text m (Producer Text m r)
splits :: (Monad m) => Char -> Producer Text m r :~> FreeT (Producer Text m) m r
splitsWith :: (Monad m) => (Char -> Bool) -> Producer Text m r -> FreeT (Producer Text m) m r
stripStart :: Monad m => Pipe Text Text m r
take :: (Monad m, Integral a) => a -> Pipe Text Text m ()
takeWhile :: (Monad m) => (Char -> Bool) -> Pipe Text Text m ()
toCaseFold :: Monad m => Pipe Text Text m r
toLazy :: Producer Text Identity () -> TL.Text
toLazyM :: (Monad m) => Producer Text m () -> m TL.Text
toLower :: Monad m => Pipe Text Text m r
toUpper :: Monad m => Pipe Text Text m r
unDrawChar :: (Monad m) => Char -> Parser Text m ()
unlines :: Monad m => FreeT (Producer Text m) m r :~> Producer Text m r
unpack :: Monad m => Producer Text m r :~> Producer Char m r
unwords :: Monad m => FreeT (Producer Text m) m r :~> Producer Text m r
word :: (Monad m) => Producer Text m r :~> Producer Text m (Producer Text m r)
words :: (Monad m) => Producer Text m r :~> FreeT (Producer Text m) m r
```
