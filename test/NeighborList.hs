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

import Prelude as P
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

import Hasdy.Prop
import Hasdy.Spatial (wrapBox)
import Hasdy.Vectors
import Hasdy.Neighbor.Fast

data System = System {box :: Vec3' Float,
                      cell :: Vec3' Float,
                      positions :: [Vec3' Float]} deriving Show

triplify (x:y:z:rest) = (x, y, z):triplify rest
triplify _ = []

bigWrap box position = position `minus3'` offset
  where
    offset = boxShifts `times3'` box
    boxShifts = map3' (P.fromIntegral . P.round) . (`div3'` box) $ position

instance Arbitrary System where
  arbitrary = do
    box' <- vectorOf 3 (choose (1e-3, 1e3))
    cellDim' <- vectorOf 3 (choose (3, 5)) :: Gen [Int]
    positions' <- listOf arbitrary :: Gen [Float]
    let box = map3' abs . head . triplify $ box'
        cellDim = map3' (max 3) . head . triplify $ cellDim'
        cell = box `div3'` map3' P.fromIntegral cellDim
        positions = bigWrap box <$> triplify positions'
    return $ System box cell positions

  shrink (System box cell positions) = System box cell <$> shrink positions

system2NList::System->([Int], [Int], [Int])
system2NList (System box cell positions) = (idxI', idxJ', segments')
  where
    idxI' = A.toList . run $ idxI
    idxJ' = A.toList . run $ idxJ
    segments' = A.toList . run $ segments
    (NList' idxI idxJ segments, _) = buildNList' True cell' box' positions'
    cell' = constToSingleProp cell
    box' = constToSingleProp box
    positions' = A.use . A.fromList (Z:.length positions) $ positions

-- | When we do a segmented fold over particles, we should have the
-- same number of indices for particle i as for particle j, and the
-- sum of the segments array should be the total number of neighbor
-- relations.
prop_consistent_sizes::System->Bool
prop_consistent_sizes sys = length idxI == length idxJ &&
                            length idxI == totalSize &&
                            length idxJ == totalSize
  where
    (idxI, idxJ, segments) = system2NList sys
    totalSize = P.sum segments

-- | The neighbor relation should be symmetric: if i is a neighbor of
-- j, then j should also be a neighbor of i.
prop_symmetric::System->Bool
prop_symmetric sys = neighbors_i == neighbors_j
  where
    (idxI, idxJ, _) = system2NList sys
    neighbors_i = Set.fromList $ P.zip idxI idxJ
    neighbors_j = Set.fromList $ P.zip idxJ idxI

-- | For each particle, the set of neighbors within a distance of the
-- minimum value of the cell size among the three dimensions should be
-- a subset of those calculated by the neighbor list, or else we have
-- missed some interaction.
prop_distance::System->Property
prop_distance sys = label (show (Set.size sphereNeighbors) P.++ " sphere neighbors") $ sphereNeighbors `Set.isSubsetOf` nlNeighbors
  where
    sphereNeighbors = Set.fromList . A.toList . run . A.map A.fst . A.filter withinSphere $ allPairs
    withinSphere = (<* rmin*rmin) . (\r -> r `dot3` r) . wrapBox (the . unSingleProp $ box') id . A.snd
    rmin = A.the . A.unit . A.constant . fold3' min $ cell sys

    positions' = A.gather oldIndices positions''
    (_, oldIndices) = buildNList' True cell' box' positions''
    cell' = constToSingleProp $ cell sys
    box' = constToSingleProp $ box sys
    positions'' = A.use . A.fromList (Z:.n) $ positions sys
    n = length . positions $ sys

    allPairs = A.filter (A.uncurry (/=*) . A.fst) . A.flatten $ A.zip (A.generate (A.constant (Z:.n:.n)) A.unindex2)
               (A.zipWith minus3 (A.replicate (A.constant $ Z:.All:.n) positions') (A.replicate (A.constant $ Z:.n:.All) positions'))
    nlNeighbors = Set.fromList $ P.zip idxI idxJ
    (idxI, idxJ, _) = system2NList sys

main = do
  quickCheck prop_consistent_sizes
  quickCheck prop_symmetric
  quickCheck prop_distance
