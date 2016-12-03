{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures #-}

module Forestay.Pretty (
    module X
  , textLit
  , groupDoc
  , listDoc
  , catDoc
  , (>+<)
  , (>++<)
  , (>$<)
  , (>$$<)
  , (>/<)
  , (>//<)
)   where

import Protolude.Lifted

import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text as X hiding
    ( text
    , group
    , list
    , cat
    , bool -- use pretty
    , empty -- use mempty
    , (<>) -- use mappend
    , (<+>)
    , (<++>)
    , (<$>)
    , (<$$>)
    , (</>)
    , (<//>)
    )

textLit :: LText -> Doc
textLit = PP.text
{-# INLINE textLit #-}

groupDoc :: Doc -> Doc
groupDoc = PP.group
{-# INLINE groupDoc #-}

listDoc :: [Doc] -> Doc
listDoc = PP.list
{-# INLINE listDoc #-}

catDoc :: [Doc] -> Doc
catDoc = PP.cat
{-# INLINE catDoc #-}

(>+<) :: Doc -> Doc -> Doc
(>+<) = (PP.<+>)
{-# INLINE (>+<) #-}
infixr 6 >+<

(>++<) :: Doc -> Doc -> Doc
(>++<) = (PP.<++>)
{-# INLINE (>++<) #-}
infixr 6 >++<

(>$<) :: Doc -> Doc -> Doc
(>$<) = (PP.<$>)
{-# INLINE (>$<) #-}
infixr 5 >$<

(>$$<) :: Doc -> Doc -> Doc
(>$$<) = (PP.<$$>)
{-# INLINE (>$$<) #-}
infixr 5 >$$<

(>/<) :: Doc -> Doc -> Doc
(>/<) = (PP.</>)
{-# INLINE (>/<) #-}
infixr 5 >/<

(>//<) :: Doc -> Doc -> Doc
(>//<) = (PP.<//>)
{-# INLINE (>//<) #-}
infixr 5 >//<

