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

{-# LANGUAGE TypeOperators #-}
module Hasdy.Neighbor.Slow where

import Data.Array.Accelerate as A
import Data.Map as M
import Hasdy.Prop (PerParticleProp(..), unPerParticleProp)
import Hasdy.Types (ParticleType)

-- | N^2 naive neighbor calculation
foldNeighbors::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                   (Exp c->Exp c->Exp c)->Exp c->ParticleType->ParticleType->
                   PerParticleProp a->PerParticleProp b->PerParticleProp c
foldNeighbors f combx x0 typeA typeB propA propB
  | typeA == typeB = PerParticleProp $ M.fromList [(typeA, xAA)]
  | otherwise = PerParticleProp $ M.fromList [(typeA, xAB), (typeB, xBA)]
  where
    -- Total properties for particles of type A due to B and for B due to A
    xAB = A.fold combx x0 xAB'
    xBA = A.fold combx x0 xBA'
    xAA = A.fold combx x0 xAA'

    -- Individual terms of AB and BA interactions
    xAB' = zipWithPairs f (propA' M.! typeA) (propB' M.! typeB)
    xBA' = zipWithPairs f (propA' M.! typeB) (propB' M.! typeA)
    xAA' = A.backpermute (nonDiagonalShape $ propA' M.! typeA) nonDiagonalIdx xAB'

    -- Remove one layer of wrapping for convenience
    propA' = unPerParticleProp $ propA
    propB' = unPerParticleProp $ propB

    -- Apply a function to all pairs of elements in two vectors
    zipWithPairs f x y = A.zipWith f x' y'
      where
        x' = A.replicate (A.lift $ Z:.(A.size y):.All) x
        y' = A.replicate (A.lift $ Z:.All:.(A.size x)) y

    -- Tricks for off-diagonal indices for A-A self-interactions
    nonDiagonalShape::Elt a=>Acc (A.Vector a)->Exp DIM2
    nonDiagonalShape arr = A.lift (Z:.i:.i - 1) :: Exp DIM2
      where
        (Z:.i) = A.unlift $ A.shape arr :: (Z:.Exp Int)
    nonDiagonalIdx idx = A.lift (Z:.i:.j')
      where
        (Z:.i:.j) = A.unlift idx
        j' = j <* i? (j, j + 1)
