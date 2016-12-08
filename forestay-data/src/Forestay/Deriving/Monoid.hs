{-# LANGUAGE TypeOperators, FlexibleContexts #-}

{-|
Module      : Forestay.Deriving.Monoid
Description : Derive Monoids
Maintainer  : David Baynard <davidbaynard@gmail.com>

Based on Michael Snoyman’s Gist <https://gist.github.com/snoyberg/3769191>

-}

module Forestay.Deriving.Monoid (
    defMempty
  , defMappend
)   where

-- Use GHC 7.4's Generic class for creating Monoid instances
import GHC.Generics

-- Generic version of Monoid. We'll need to create an instance for each of the
-- Generic types.
class GMonoid f where
    gmempty :: f a
    gmappend :: f a -> f a -> f a

-- Nullary constructors. The instance is simple: mempty is the constructor,
-- mappend is the constructor.
instance GMonoid U1 where
    gmempty = U1
    gmappend U1 U1 = U1

-- Products
instance (GMonoid a, GMonoid b) => GMonoid (a :*: b) where
    -- Product of two gmempty values
    gmempty = gmempty :*: gmempty

    -- Apply gmappend recursively to the left and right and create a new
    -- product.
    gmappend (a :*: x) (b :*: y) = gmappend a b :*: gmappend x y

{-

The following will allow sum types to have Monoid instances. However, if the
two arguments to mappend use different constructors, it will throw out all of
the data from the second argument. As such, I consider this instance mostly
broken, and have commented it out.

What's really nice is that GHC will now be able to given a compile error if you
try and use the generic monoid code for a sum type.

instance (GMonoid a, GMonoid b) => GMonoid (a :+: b) where
    gmempty = L1 gmempty
    gmappend (L1 x) (L1 y) = L1 (gmappend x y)
    gmappend (R1 x) (R1 y) = R1 (gmappend x y)
    gmappend x _ = x
-}

-- Metadata: just a passthrough
instance GMonoid a => GMonoid (M1 i c a) where
    gmempty = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y

-- Arguments: now use the real Monoid methods. We're essentially just
-- wrapping/unwrapping here.
--
-- Note that this forces all of the fields in our datatype to be instances of
-- Monoid, which is what we should expect.
instance Monoid a => GMonoid (K1 i a) where
    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y

-- Default implementations of mempty and mappend using gmempty and gmappend.
-- All we do is use @to@ and @from@ to wrap and unwrap.
defMempty :: (Generic a, GMonoid (Rep a)) => a
defMempty = to gmempty
{-# INLINE defMempty #-}

defMappend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
defMappend x y = to $ from x `gmappend` from y
{-# INLINE defMappend #-}


