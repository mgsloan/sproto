{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto.Types (
    WireId(..), Iso(..), IsoMap(..),
    flipIso, (<<<), (>>>),
    isoLookup, isoFromList, lookupFrom, lookupTo, isoToList, isoAs, isoBs
  ) where

import Data.Word
import Data.Bits
import Data.Ranged (DiscreteOrdered(..))
import qualified Data.Map as M

newtype WireId = WireId Word32 deriving (Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

instance DiscreteOrdered WireId where
    adjacent x y = adjacent (toInteger x) (toInteger y)
    adjacentBelow 1 = Nothing
    adjacentBelow x = Just (x - 1)

data Iso a b = Iso {to :: a -> b, from :: b -> a}
flipIso (Iso f g) = Iso g f

infixr 1 >>>, <<<

(<<<) :: Iso b c -> Iso a b -> Iso a c
(Iso f1 g1) <<< (Iso f2 g2) = Iso (f1 . f2) (g2 . g1)

(>>>) :: Iso a b -> Iso b c -> Iso a c
(Iso f1 g1) >>> (Iso f2 g2) = Iso (f2 . f1) (g1 . g2)

isoMap :: (Iso a b) -> (Iso [a] [b])
isoMap f = Iso (map $ to f) (map $ from f)

data (Ord a, Ord b) => IsoMap a b = IsoMap (M.Map a b) (M.Map b a)

instance (Show a, Show b, Ord a, Ord b) => Show (IsoMap a b) where
  show (IsoMap x _) = show x

isoLookup :: (Ord a, Ord b) => (Maybe b -> b) -> (Maybe a -> a) -> IsoMap a b -> Iso a b
isoLookup f g (IsoMap m1 m2) = Iso (\x -> f . M.lookup x $ m1)
                                   (\x -> g . M.lookup x $ m2)

isoFromList :: (Ord a, Ord b) => [(a, b)] -> IsoMap a b
isoFromList x = IsoMap (M.fromList x) (M.fromList $ map (\(a,b)->(b,a)) x)

isoToList :: (Ord a, Ord b) => IsoMap a b -> [(a, b)]
isoToList (IsoMap m1 _) = M.toList m1

isoAs :: (Ord a, Ord b) => IsoMap a b -> [a]
isoAs (IsoMap m1 _) = M.keys m1

isoBs :: (Ord a, Ord b) => IsoMap a b -> [b]
isoBs (IsoMap _ m2) = M.keys m2

lookupTo :: (Ord a, Ord b) => a -> IsoMap a b -> Maybe b
lookupTo x (IsoMap m _) = M.lookup x m
lookupFrom :: (Ord a, Ord b) => b -> IsoMap a b -> Maybe a
lookupFrom x (IsoMap _ m) = M.lookup x m