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

module Hasdy.Potentials.LJ where

import Data.Array.Accelerate as A

import Hasdy.Vectors

data LJ r = LJ {epsilon::Acc (A.Scalar r),
                sigma::Acc (A.Scalar r)}

ljForce::(Elt r, IsFloating r)=>LJ r->Vec3 r->Vec3 r
ljForce params r = map3 (prefactor*) r
  where
    r2 = dot3 r r
    r4 = r2*r2
    r8 = r4*r4
    r6 = r4*r2
    prefactor = 48*epsilon'*sig6/r8*(1 - 2*sig6/r6)
    sig3 = sigma'*sigma'*sigma'
    sig6 = sig3*sig3
    sigma' = the . sigma $ params
    epsilon' = the . epsilon $ params

ljPotential::(Elt r, IsFloating r)=>LJ r->Vec3 r->Exp r
ljPotential params r = 4*epsilon'*(sig12f - sig6f)
  where
    epsilon' = the . epsilon $ params
    sig12f = sig6f*sig6f
    sig6f = sig*sig*sig*sig*sig*sig/r6
    sig = the . sigma $ params
    r6 = r2*r2*r2
    r2 = dot3 r r
