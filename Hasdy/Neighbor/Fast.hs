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

module Hasdy.Neighbor.Fast where

import Data.Array.Accelerate as A
import qualified Data.Map as M

import Data.Array.Accelerate.HasdyContrib as HC
import Hasdy.Prop
import Hasdy.Types
import Hasdy.Utils
import Hasdy.Utils.Sort
import Hasdy.Vectors

data NList = NList {unNList :: M.Map ParticleType NList'}
data NList' = NList' {nlIdxi :: Acc (A.Vector Int),
                      nlIdxj :: Acc (A.Vector Int),
                      nlSegments :: Acc (A.Vector Int)}

zOrder::Vec3 Int->Exp Int
-- zOrder idx = interleave3 z y x
--   where
--     (x, y, z) = A.unlift idx
zOrder idx = (x*(2^20) + y*(2^10) + z)
  where
    (x, y, z) = A.unlift idx

gridOffsets::Acc (A.Vector (Vec3' Int))
gridOffsets = A.use $ A.fromList (Z:.27)
              [(x, y, z) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1]]

buildNList::(Elt r, IsFloating r)=>Bool->SingleProp (Vec3' r)->SingleProp (Vec3' r)->
            PerParticleProp (Vec3' r)->(NList, PerParticleProp Int)
buildNList skipSelf cell box positions = (nlist, oldIndices)
  where
    nlist = NList . M.map Prelude.fst $ result
    oldIndices = PerParticleProp $ M.map Prelude.snd result
    result = M.map (buildNList' skipSelf cell box) . unPerParticleProp $ positions

buildNList' skipSelf cell box positions = (NList' idxI idxJ segments, oldIndices)
  where
    cell' = the . unSingleProp $ cell
    box' = the . unSingleProp $ box

    -- Vector and components for box size in cells
    dim = map3 A.floor $ box' `div3` cell'
    cell'' = box' `div3` map3 A.fromIntegral dim
    (dim0, dim1, dim2) = A.unlift dim :: Vec3' (Exp Int)
    gridSize = fold3 (*) dim

    -- Assign each particle to a cell based on its position; len == numParticles
    cells::Acc (A.Vector (Vec3' Int))
    cells = A.map (map3 A.floor . (`div3` cell'') . (`plus3` scale3 0.5 box')) positions

    -- Sort particles based on the z-order of cell index; len == numParticles
    unsortedZOrders = A.map zOrder cells
    cellZOrders::Acc (A.Vector Int)
    oldIndices::Acc (A.Vector Int)
    (cellZOrders, _, oldIndices) = sort id unsortedZOrders

    -- Histogram the cells to calculate the number of particles in each cell; len == numCells
    cellSizes::Acc (A.Vector Int)
    cellSizes = A.permute (+) (A.fill (A.lift $ Z:.gridSize) 0)
                (A.index1 . dot3 cumulativeGridSize . (cells A.!)) (A.fill (A.shape cells) 1)

    -- Beginning indices for each cell; len == numCells
    cellStarts = A.prescanl (+) 0 cellSizes

    -- vector to dot with to convert to a flat index in the 3D grid
    cumulativeGridSize = A.lift (dim1*dim2, dim1, 1::Exp Int) :: Vec3 Int

    -- Cells and cell indices per-particle
    particleCells = A.gather oldIndices cells
    particleCells' = A.map (dot3 cumulativeGridSize) particleCells

    -- starts and sizes of per-particle cells
    particleCellStarts = A.gather particleCells' cellStarts
    particleCellSizes = A.gather particleCells' cellSizes

    -- extended cell (cell + its 26 neighbor) stats
    particleExtendedCells = A.flatten . A.map (dot3 cumulativeGridSize . (Hasdy.Vectors.zipWith3 (flip mod) dim)) $
                            A.zipWith plus3 (A.replicate (A.lift $ Z:.A.size particleCells:.All) gridOffsets)
                            (A.replicate (A.constant $ Z:.All:.(27::Int)) particleCells)
    particleExtendedStarts = A.gather particleExtendedCells cellStarts
    particleExtendedSizes = A.gather particleExtendedCells cellSizes

    -- intermediate results
    idxI' = HC.repeat segments' (A.generate (A.shape positions) unindex1)
    idxJ' = unfoldSeg (+1) particleExtendedSizes particleExtendedStarts
    segments' = A.fold (+) 0 $ A.reshape (A.lift $ Z:.A.size particleCells:.(27::Int)) particleExtendedSizes

    -- final results
    (idxI, idxJ)
      | skipSelf = A.unzip . A.filter (A.uncurry (/=*)) $ A.zip idxI' idxJ'
      | otherwise = (idxI', idxJ')
    segments
      | skipSelf = A.map (\x -> max 0 (x - 1)) segments'
      | otherwise = segments'

-- | mostly garbage implementation specialized
foldNeighbors::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                   (Exp c->Exp c->Exp c)->Exp c->NList->ParticleType->ParticleType->
                   PerParticleProp a->PerParticleProp b->PerParticleProp c
foldNeighbors f combx x0 nlist typeA typeB propi propj
  | typeA == typeB = PerParticleProp $ M.fromList [(typeA, xAA)]
  | otherwise = undefined
  where
    (NList' idxI idxJ segments) = (flip (M.!) $ typeA) . unNList $ nlist
    xAA = foldSeg combx x0 xij segments
    xij = A.zipWith f xi xj
    xi = A.gather idxI ((flip (M.!) $ typeA) . unPerParticleProp $ propi)
    xj = A.gather idxJ ((flip (M.!) $ typeA) . unPerParticleProp $ propj)
