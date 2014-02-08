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

import Control.Applicative ((<$>))
import Data.Array.Accelerate as A
import qualified Data.Map as M

import Data.Array.Accelerate.HasdyContrib as HC
import Hasdy.Prop
import Hasdy.Prop.Bundle
import Hasdy.Prop.Math (minusp3)
import Hasdy.Types
import Hasdy.Utils
import Hasdy.Utils.Sort
import Hasdy.Vectors

data NList = NList {unNList :: M.Map ParticleType SNList}
data NList' = NList' {unNList' :: M.Map ParticleType SNList'}

data SNList = SNList {nlIdxi :: Acc (A.Vector Int),
                      nlIdxj :: Acc (A.Vector Int),
                      nlSegments :: Acc (A.Vector Int)}

data SNList' = SNList' {nlIdxi' :: A.Vector Int,
                        nlIdxj' :: A.Vector Int,
                        nlSegments' :: A.Vector Int}

gridOrder::Vec3 Int->Exp Int
gridOrder idx = (x*(2^18) + y*(2^9) + z)
  where
    (x, y, z) = A.unlift idx

gridOffsets::Acc (A.Vector (Vec3' Int))
gridOffsets = A.use $ A.fromList (Z:.27)
              [(x, y, z) | x <- [-1, 0, 1], y <- [-1, 0, 1], z <- [-1, 0, 1]]

buildNList::(Elt r, IsFloating r)=>Bool->SingleProp r->SingleProp (Vec3' r)->
            PerParticleProp (Vec3' r)->(NList, PerParticleProp Int)
buildNList skipSelf cellR box positions = (nlist, oldIndices)
  where
    nlist = NList . M.map Prelude.fst $ result
    oldIndices = PerParticleProp $ M.map Prelude.snd result
    result = M.map (buildNList' skipSelf cellR box) . unPerParticleProp $ positions

buildNList' skipSelf cellR box positions = (SNList idxI idxJ segments, oldIndices)
  where
    cellR' = the . unSingleProp $ cellR
    box' = the . unSingleProp $ box

    -- Vector and components for box size in cells
    dim = map3 (A.floor . (/cellR')) box'
    cell'' = box' `div3` map3 A.fromIntegral dim
    (dim0, dim1, dim2) = A.unlift dim :: Vec3' (Exp Int)
    gridSize = fold3 (*) dim

    -- Assign each particle to a cell based on its position; len == numParticles
    cells::Acc (A.Vector (Vec3' Int))
    cells = A.map (map3 A.truncate . (`div3` cell'') . (`plus3` scale3 0.5 box')) positions

    -- Sort particles based on the z-order of cell index; len == numParticles
    unsortedOrders = A.map gridOrder cells
    cellOrders::Acc (A.Vector Int)
    oldIndices::Acc (A.Vector Int)
    (cellOrders, _, oldIndices) = sort id unsortedOrders

    -- Histogram the cells to calculate the number of particles in each cell; len == numCells
    cellSizes::Acc (A.Vector Int)
    cellSizes = A.permute (+) (A.fill (A.lift $ Z:.gridSize) 0)
                (A.index1 . dot3 cumulativeGridSize . (cells A.!)) (A.fill (A.shape cells) 1)

    -- Beginning indices for each cell; len == numCells
    cellStarts = A.prescanl (+) 0 cellSizes

    -- vector to dot with to convert to a flat index in the 3D grid
    cumulativeGridSize = A.lift (dim1*dim2, dim2, 1::Exp Int) :: Vec3 Int

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
    idxJ' = unfoldSeg (\x y -> 1 + x + y) 0 particleExtendedSizes particleExtendedStarts
    segments' = A.fold (+) 0 $ A.reshape (A.lift $ Z:.A.size particleCells:.(27::Int)) particleExtendedSizes

    -- final results
    (idxI, idxJ)
      | skipSelf = A.unzip . A.filter (A.uncurry (/=*)) $ A.zip idxI' idxJ'
      | otherwise = (idxI', idxJ')
    segments
      | skipSelf = A.map (\x -> max 0 (x - 1)) segments'
      | otherwise = segments'

-- | Choose the left neighbor list if a condition is true, else choose
-- the right neighbor list
chooseNList::PerTypeProp Bool->(NList, NList)->NList
chooseNList cond (left, right) = NList $ M.intersectionWith chooseSNList cond' pairs
  where
    cond' = the <$> unPerTypeProp cond
    (left', right') = (unNList left, unNList right)
    pairs = M.intersectionWith (,) left' right'

chooseSNList::Exp Bool->(SNList, SNList)->SNList
chooseSNList cond ((SNList lefti leftj leftseg), (SNList righti rightj rightseg)) =
  SNList idxi idxj seg
  where
    idxi = cond ?| (lefti, righti)
    idxj = cond ?| (leftj, rightj)
    seg = cond ?| (leftseg, rightseg)

-- | Rebuild neighbor list if particles have moved farther than a given distance
maybeRebuildNList::(Elt r, IsFloating r)=>Bool->SingleProp r->SingleProp r->SingleProp (Vec3' r)->
                   PerParticleProp (Vec3' r)->PerParticleProp (Vec3' r)->NList->
                   (PerTypeProp Bool, NList, PerParticleProp Int)
maybeRebuildNList skipSelf cellR maxDistance box oldPositions positions old =
  (rebuilt, nlist', oldIndices)
  where
    rebuilt = reducePerParticle (||*) (A.constant False) overCutoffs
    overCutoffs = perParticleMap (>* rcut*rcut) rsq
    rsq = perParticleMap (\x -> x `dot3` x) $ positions `minusp3` oldPositions
    rcut = the . unSingleProp $ maxDistance

    nlist' = chooseNList rebuilt (new, old)

    (new, newIdx) = buildNList skipSelf cellR box positions

    oldIndices = choosePerParticle rebuilt (newIdx, trivIdx)
    trivIdx = overPerParticle (fmap (\x -> A.generate (A.shape x) A.unindex1)) oldPositions

-- | mostly garbage implementation specialized
foldNeighbors::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                   (Exp c->Exp c->Exp c)->Exp c->NList->ParticleType->ParticleType->
                   PerParticleProp a->PerParticleProp b->PerParticleProp c
foldNeighbors f combx x0 nlist typeA typeB propi propj
  | typeA == typeB = PerParticleProp $ M.fromList [(typeA, xAA)]
  | otherwise = undefined
  where
    (SNList idxI idxJ segments) = (flip (M.!) $ typeA) . unNList $ nlist
    xAA = foldSeg combx x0 xij segments
    xij = A.zipWith f xi xj
    xi = A.gather idxI ((flip (M.!) $ typeA) . unPerParticleProp $ propi)
    xj = A.gather idxJ ((flip (M.!) $ typeA) . unPerParticleProp $ propj)

takeNList::Arrays c=>ParticleType->
           Bundle (a, NList) (Acc c)->
           Bundle (a, NList) (Acc (c, (A.Vector Int, A.Vector Int, A.Vector Int)))
takeNList typ (Bundle (a, nlist) c) = Bundle (a, nlist) (A.lift (c, (ixi, ixj, seg)))
  where
    ixi = nlIdxi $ unNList nlist M.! typ
    ixj = nlIdxj $ unNList nlist M.! typ
    seg = nlSegments $ unNList nlist M.! typ

takeNList'::Arrays c=>ParticleType->
            Bundle (a, NList') c->
            Bundle (a, NList') (c, (A.Vector Int, A.Vector Int, A.Vector Int))
takeNList' typ (Bundle (a, nlist) c) = Bundle (a, nlist) (c, (ixi, ixj, seg))
  where
    ixi = nlIdxi' $ unNList' nlist M.! typ
    ixj = nlIdxj' $ unNList' nlist M.! typ
    seg = nlSegments' $ unNList' nlist M.! typ

giveNList::Arrays c=>ParticleType->
           Bundle (a, NList) (Acc (c, (A.Vector Int, A.Vector Int, A.Vector Int)))->
           Bundle (a, NList) (Acc c)
giveNList typ (Bundle (a, nlist) v) = Bundle (a, nlist') (A.fst v)
  where
    nlist' = NList . M.insert typ newnl . unNList $ nlist
    newnl = SNList ixi ixj seg
    (ixi, ixj, seg) = A.unlift . A.snd $ v

giveNList'::Arrays c=>ParticleType->
            Bundle (a, NList') (c, (A.Vector Int, A.Vector Int, A.Vector Int))->
            Bundle (a, NList') c
giveNList' typ (Bundle (a, nlist) v) = Bundle (a, nlist') (Prelude.fst v)
  where
    nlist' = NList' . M.insert typ newnl . unNList' $ nlist
    newnl = SNList' ixi ixj seg
    (ixi, ixj, seg) = Prelude.snd v
