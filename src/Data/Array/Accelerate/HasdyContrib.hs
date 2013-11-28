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

module Data.Array.Accelerate.HasdyContrib where

import Data.Array.Accelerate as A

-- | Repeat each value in a sequence a given number of times.
--   repeat [1, 0, 4] [1.0, 1.2, 1.3] == [1.0, 1.3, 1.3, 1.3, 1.3]
repeat::Elt a=>Acc (A.Vector Int)->Acc (A.Vector a)->Acc (A.Vector a)
repeat ns xs = gather idx xs
  where
    (starts, outputSize) = scanl' (+) 0 ns
    size = index1 . the $ outputSize
    range = A.generate (A.shape ns) unindex1
    idx' = scatterIf starts ns (>* 0) (fill size 0) range
    idx = A.scanl1 max idx'

-- | "Unfold" a list of initial values into segments
--   unfoldSeg (+2) [3, 1, 0, 2] [100, 0, 200, 300] == [100, 102, 104, 0, 300, 302]
unfoldSeg::Elt a=>(Exp a->Exp a)->Acc (A.Vector Int)->Acc (A.Vector a)->Acc (A.Vector a)
unfoldSeg f ns xs = scanl1Seg (\x _ -> f x) scattered ns
  where
    (starts, outputSize) = scanl' (+) 0 ns
    size = index1 . the $ outputSize
    blank = fill size (xs A.!! 0)
    scattered = scatterIf starts ns (>* 0) blank xs
