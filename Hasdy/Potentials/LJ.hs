module Hasdy.Potentials.LJ where

import Data.Array.Accelerate as A

import Hasdy.Vectors

data LJ r = LJ {epsilon::Acc (A.Scalar r),
                sigma::Acc (A.Scalar r),
                rcut::Acc (A.Scalar r)}

ljForce::(Elt r, IsFloating r)=>LJ r->Exp (Vec3 r)->Exp (Vec3 r)
ljForce params r = map3 (prefactor*) r
  where
    prefactor = 24*epsilon'*sig6/r22/r22*(1 - 2*sig6/r22/r2)
    r2 = dot3 r r
    r22 = r2*r2
    sig3 = sigma'*sigma'*sigma'
    sig6 = sig3*sig3
    sigma' = the . sigma $ params
    epsilon' = the . epsilon $ params

ljPotential::(Elt r, IsFloating r)=>LJ r->Exp (Vec3 r)->Exp r
ljPotential params r = epsilon'*(sig12f - sig6f)
  where
    epsilon' = (A.constant 4)*(the . epsilon $ params)
    sig12f = sig6f*sig6f
    sig6f = sig*sig*sig*sig*sig*sig/r23
    sig = the . sigma $ params
    r23 = r2*r2*r2
    r2 = dot3 r r
