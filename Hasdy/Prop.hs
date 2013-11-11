module Hasdy.Prop where

import Control.Applicative ((<$>))
import Data.Array.Accelerate as A
import Data.Map as M

import Hasdy.Types

newtype PerParticleProp a =
  PerParticleProp {unPerParticleProp::M.Map ParticleType (Acc (A.Vector a))}
newtype PerParticleProp' a =
  PerParticleProp' {unPerParticleProp'::M.Map ParticleType (A.Vector a)}

perParticleMap::(Elt a, Elt b)=>(Exp a->Exp b)->PerParticleProp a->PerParticleProp b
perParticleMap f (PerParticleProp x) = PerParticleProp $ M.map (A.map f) x

perParticleZipWith::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                    PerParticleProp a->PerParticleProp b->PerParticleProp c
perParticleZipWith f (PerParticleProp x) (PerParticleProp y) = PerParticleProp z
  where
    z = A.map (A.uncurry f) <$> M.intersectionWith A.zip x y

instance (Elt a, IsNum a)=>Num (PerParticleProp a) where
  fromInteger _ = undefined
  (+) = perParticleZipWith (+)
  (-) = perParticleZipWith (-)
  (*) = perParticleZipWith (*)
  negate = perParticleMap negate
  abs = perParticleMap abs
  signum = perParticleMap signum

newtype PerTypeProp a =
  PerTypeProp {unPerTypeProp::M.Map ParticleType (Acc (A.Scalar a))}
newtype PerTypeProp' a =
  PerTypeProp' {unPerTypeProp'::M.Map ParticleType (A.Scalar a)}

newtype SingleProp a = SingleProp {unSingleProp::Acc (A.Scalar a)}
newtype SingleProp' a = SingleProp' {unSingleProp'::A.Scalar a}

singleToParticleProp::Elt a=>SingleProp a->PerParticleProp a->PerParticleProp a
singleToParticleProp (SingleProp x) (PerParticleProp template) = PerParticleProp x'
  where
    x' = M.map (broadcast x) template
    broadcast from to = A.replicate (A.lift $ Z:.(A.size to)) from

-- make the symbolic value a newtype
-- make an ADT with PerParticleProp being the symbolic value and PerParticleProp' be the evaluated value or so
-- make it a typeclass with sym::PerParticleProp a b=>a->Acc b and eval::whatever

-- however that works out, need num instances for all the props
