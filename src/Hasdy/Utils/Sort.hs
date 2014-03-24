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

msb::(Elt a, IsFloating a)=>Exp a->Exp Int
msb = A.ceiling . logBase 2

-- | simple radix sort. Takes a function by which to generate keys
-- (keys should be >0 to be sorted "properly") and a vector of values
-- and returns (the sorted vector of keys, the sorted vector of
-- values, and the sorted vector of old indices suitable for a gather
-- operation).
sort::Elt a=>(Exp a->Exp Int)->Acc (A.Vector a)->
      (Acc (A.Vector Int), Acc (A.Vector a), Acc (A.Vector Int))
sort key v = (keys, values, oldIndices)
  where
    result = A.awhile iterCond sort' v0s

    keys = P.fst . A.unzip . A.snd $ result
    values = P.fst . A.unzip . P.snd . A.unzip . A.snd $ result
    oldIndices = P.snd . A.unzip . P.snd . A.unzip . A.snd $ result

    iterCond = A.unit . (<=* maxBit) . the . A.fst
    maxBit = msb . (A.fromIntegral::Exp Int->Exp Float) . the . A.maximum $ key0s
    key0s = A.map key v
    v0s = A.lift (A.unit . A.constant $ 0,
                  A.zip key0s $ A.zip v (A.generate (A.shape v) unindex1))

    -- sort' performs a single sorting step
    sort'::Elt a=>Acc (A.Scalar Int, A.Vector (Int, (a, Int)))->
           Acc (A.Scalar Int, A.Vector (Int, (a, Int)))
    sort' vs = A.lift (A.unit (n+1), A.scatter newIndices vs' vs')
      where
        n = the . A.fst $ vs
        vs' = A.snd vs
        idx = A.prescanl (+) 0 bits
        bits = A.map (A.boolToInt . (flip A.testBit $ n) . A.fst) vs'
        unbits = A.map (1-) bits
        falseIndices = A.init $ A.scanl (+) 0 unbits
        numFalses = (A.reverse falseIndices A.!! 0) + (A.reverse unbits A.!! 0)
        trueIndices = A.map (+numFalses) $ A.zipWith (-) (A.generate (A.shape bits) unindex1) falseIndices
        newIndices = A.map choose $ A.zip3 bits trueIndices falseIndices
        choose v =
          (b ==* 1) ? (x, y)
          where
            (b, x, y) = A.unlift v :: (Exp Int, Exp Int, Exp Int)
