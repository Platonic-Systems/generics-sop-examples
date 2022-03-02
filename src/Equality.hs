{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# HLINT ignore "Use camelCase" #-}

module Equality where

import Data.Foldable (foldl')
import GHC.Generics qualified as GHC
import Generics.SOP

data These a b
  = This a
  | That b
  | These a b
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

-- Combinator version

geq :: forall a. (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq x y = geq' @a (from x) (from y)

geq' :: All2 Eq (Code a) => SOP I (Code a) -> SOP I (Code a) -> Bool
geq' (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All Eq)) False eqProd False c1 c2
  where
    eqProd :: All Eq xs => NP I xs -> NP I xs -> Bool
    eqProd p1 p2 =
      foldl' (&&) True $
        hcollapse $ hcliftA2 (Proxy :: Proxy Eq) eqTerm p1 p2
      where
        eqTerm :: forall a. Eq a => I a -> I a -> K Bool a
        eqTerm a b =
          K $ a == b

-- Non-combinator version

nc_geq :: forall a. (Generic a, SumEq (Code a)) => a -> a -> Bool
nc_geq x y = nc_geq' @a (unSOP . from $ x) (unSOP . from $ y)

nc_geq' :: SumEq (Code a) => NS (NP I) (Code a) -> NS (NP I) (Code a) -> Bool
nc_geq' = sumEq

class SumEq xss where
  sumEq :: NS (NP I) xss -> NS (NP I) xss -> Bool

instance SumEq '[] where
  sumEq = \case

instance (SumEq xss, ProdEq xs) => SumEq (xs ': xss) where
  sumEq (S x) (S y) = sumEq x y
  sumEq (Z x) (Z y) = prodEq x y
  sumEq _ _ = False

class ProdEq xs where
  prodEq :: NP I xs -> NP I xs -> Bool

instance ProdEq '[] where
  prodEq Nil Nil = True

instance (Eq x, ProdEq xs) => ProdEq (x ': xs) where
  prodEq (x :* xs) (y :* ys) = x == y && prodEq xs ys
