module Hasdy.Potentials.Sigmoidal where

import Data.Array.Accelerate as A

import Hasdy.Vectors

data Sigmoidal r = Sigmoidal {epsilon::Acc (A.Scalar r),
                              r0::Acc (A.Scalar r),
                              k::Acc (A.Scalar r),
                              rcut::Acc (A.Scalar r)}

sigmoidalForce::(Elt r, IsFloating r)=>Sigmoidal r->Vec3 r->Vec3 r
sigmoidalForce params r = map3 (prefactor*) r
  where
    prefactor = 2*epsilon'*k'*u*(1 - u)
    epsilon' = the . epsilon $ params
    k' = the . k $ params
    u = sigmoidalPotential params r

sigmoidalPotential::(Elt r, IsFloating r)=>Sigmoidal r->Vec3 r->Exp r
sigmoidalPotential params r = epsilon'/(1 + exp(k'*(rsq - r0sq)))
  where
    epsilon' = the . epsilon $ params
    k' = the . k $ params
    r0sq = (the . r0 $ params)*(the . r0 $ params)
    rsq = dot3 r r
