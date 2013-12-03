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

--module HasdyContrib (tests) where

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Data.Array.Accelerate.HasdyContrib as HC

prop_repeat_size::[Int]->Bool
prop_repeat_size xs = repeatedSize == P.sum xs
  where
    xs' = A.fromList (Z:.length xs) xs
    repeatedSize = arraySize . arrayShape $ run1 (\x -> HC.repeat x x) xs'

prop_repeat_size' = forAll (listOf $ choose (0, 129)) prop_repeat_size

prop_repeat_values::[Int]->Bool
prop_repeat_values xs = repeatedValues == repeatedValues'
  where
    xs' = A.fromList (Z:.length xs) xs
    repeatedValues = A.toList $ run1 (\x -> HC.repeat x x) xs'
    repeatedValues' = P.concatMap (\x -> P.take x . P.repeat $ x) xs

prop_repeat_values' = forAll (listOf $ choose (0, 129)) prop_repeat_values

prop_unfold_size::[Int]->Bool
prop_unfold_size xs = unfoldedSize == P.sum xs
  where
    xs' = A.fromList (Z:.length xs) xs
    unfoldedSize = arraySize . arrayShape $ run1 (\x -> HC.unfoldSeg (\x y -> x + y + 3) 0 x x) xs'

prop_unfold_size' = forAll (listOf $ choose (0, 129)) prop_unfold_size

prop_unfold_values::[Int]->Bool
prop_unfold_values xs = unfoldedValues == unfoldedValues'
  where
    xs' = A.fromList (Z:.length xs) xs
    unfoldedValues = A.toList $ run1 (\x -> HC.unfoldSeg (\x y -> x + y + 3) 0 x x) xs'
    unfoldedValues' = P.concatMap (\x -> P.take x . P.iterate (+3) $ x) xs

prop_unfold_values' = forAll (listOf $ choose (0, 129)) prop_unfold_values

main = do
  quickCheck prop_repeat_size'
  quickCheck prop_repeat_values'

  quickCheck prop_unfold_size'
  quickCheck prop_unfold_values'
