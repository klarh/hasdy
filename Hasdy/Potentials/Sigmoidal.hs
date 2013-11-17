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
