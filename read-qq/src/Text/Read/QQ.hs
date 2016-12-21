{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Text.Read.QQ (
    read
)   where

import Prelude hiding (read)
import qualified Prelude
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

read :: forall a . (Read a, Lift a) => QuasiQuoter
read = QuasiQuoter
        { quoteExp = lift . rd
        , quoteDec = undefined
        , quotePat = undefined
        , quoteType = undefined
        }
    where
        rd :: String -> a
        rd = Prelude.read
