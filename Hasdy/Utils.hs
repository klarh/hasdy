-- Copyright 2013 Matthew Spellings

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--     http://www.apache.org/licenses/LICENSE-2.0
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Hasdy.Utils where

import Control.Applicative ((<$>))
import Data.Bits ((.|.))
import Prelude as P
import Data.Array.Accelerate as A

-- | Interleave the lower 10 bits of three Int values
interleave3::Exp Int->Exp Int->Exp Int->Exp Int
interleave3 x y z = P.foldr (.|.) 0 shifted
  where
    bitsX = A.testBit x . A.constant <$> [0..10]
    bitsY = A.testBit y . A.constant <$> [0..10]
    bitsZ = A.testBit z . A.constant <$> [0..9]
    bits = A.boolToInt <$> interleave' bitsX bitsY bitsZ
    interleave' (x:xs) (y:ys) (z:zs) = (x:y:z:(interleave' xs ys zs))
    interleave' _ _ _ = []
    shifted = P.zipWith A.shiftL bits (A.constant <$> [0..])
