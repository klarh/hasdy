module Hasdy.Neighbor.Slow where

import Data.Array.Accelerate as A
import Data.Map as M
import Hasdy.Prop (PerParticleProp(..), unPerParticleProp)
import Hasdy.Types (ParticleType)

-- N^2 naive neighbor calculation
foldNeighbors::(Elt a, Elt b)=>(Exp a->Exp a->Exp b)->
                   (Exp b->Exp b->Exp b)->Exp b->ParticleType->ParticleType->
                   PerParticleProp a->PerParticleProp b
foldNeighbors f combx x0 typeA typeB prop
  | typeA == typeB = PerParticleProp $ M.fromList [(typeA, xAB)]
  | otherwise = PerParticleProp $ M.fromList [(typeA, xAB), (typeB, xBA)]
  where
    -- Total properties for particles of type A due to B and for B due to A
    xAB = A.fold combx x0 xAB'
    xBA = A.fold combx x0 xBA'

    -- Individual terms of AB and BA interactions
    xAB' = A.zipWith f propsA propsB
    xBA' = A.transpose xAB'

    -- 2D property arrays for particles A and B
    propsA = A.replicate (A.lift $ Z:.(A.size propsB'):.All) propsA'
    propsB = A.replicate (A.lift $ Z:.(A.size propsA'):.All) propsB'

    -- 1D property arrays for particles A and B
    propsA' = (unPerParticleProp $ prop) M.! typeA
    propsB' = (unPerParticleProp $ prop) M.! typeB
