{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module GHC.Integer.GMP.TypeExt
    ( popCountInteger
    , testBitInteger
    , setBitInteger
    , clearBitInteger
    ) where

#include "MachDeps.h"

import GHC.Integer.GMP.Internals (popCountInteger, bitInteger)
import GHC.Prim (Int#)

import GHC.Integer (testBitInteger, orInteger, complementInteger, andInteger)

setBitInteger :: Integer -> Int# -> Integer
setBitInteger j i = bitInteger i `orInteger` j

clearBitInteger :: Integer -> Int# -> Integer
clearBitInteger j i = complementInteger (bitInteger i) `andInteger` j