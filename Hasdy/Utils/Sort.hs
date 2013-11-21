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

module Hasdy.Utils.Sort where

import Control.Applicative ((<$>))
import Prelude as P
import Data.Array.Accelerate as A

-- | simple radix sort. Takes a function by which to generate keys
-- (keys should be >0 to be sorted "properly") and a vector of values
-- and returns (the sorted vector of keys, the sorted vector of
-- values, and the sorted vector of old indices suitable for a gather
-- operation).
sort::Elt a=>(Exp a->Exp Int)->Acc (A.Vector a)->
         (Acc (A.Vector Int), Acc (A.Vector a), Acc (A.Vector Int))
sort key v = (keys, values, oldIndices)
  where
    result = P.foldr1 (>->) sorters v0s

    keys = P.fst . A.unzip $ result
    values = P.fst . A.unzip . P.snd . A.unzip $ result
    oldIndices = P.snd . A.unzip . P.snd . A.unzip $ result

    -- Start with a vector of (keyvalue, (value, originalIndex)) tuples
    v0s = A.zip (A.map key v) $ A.zip v (A.generate (A.shape v) unindex1)
    sorters = sort' <$> P.reverse indices

    -- sort' takes an integer bit index to sort by
    sort'::Elt a=>Exp Int->Acc (A.Vector (Int, (a, Int)))->
           Acc (A.Vector (Int, (a, Int)))
    sort' n vs = A.scatter newIndices vs vs
      where
        idx = A.prescanl (+) 0 bits
        bits = A.map (A.boolToInt . (flip A.testBit $ n) . A.fst) vs
        unbits = A.map (1-) bits
        falseIndices = A.init $ A.scanl (+) 0 unbits
        numFalses = (A.reverse falseIndices A.!! 0) + (A.reverse unbits A.!! 0)
        trueIndices = A.map (+numFalses) $ A.zipWith (-) (A.generate (A.shape bits) unindex1) falseIndices
        newIndices = A.map choose $ A.zip3 bits trueIndices falseIndices
        choose v =
          (b ==* 1) ? (x, y)
          where
            (b, x, y) = A.unlift v :: (Exp Int, Exp Int, Exp Int)

    indices = A.constant <$> [31, 30..0]
