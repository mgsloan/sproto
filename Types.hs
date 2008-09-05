{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Sproto.Types ( WireId(..) ) where
import Data.Word
import Data.Bits

newtype WireId = WireId Word32 deriving (Bits, Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)