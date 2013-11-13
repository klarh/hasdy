module Hasdy.Prop where

import Control.Applicative ((<$>))
import Data.Array.Accelerate as A
import Data.Map as M

import Hasdy.Types
import Hasdy.Vectors (Vec3'(..), plus3, minus3, times3, div3)

-- | 'PerParticleProp' is an associated list of 'ParticleType's with
-- symbolic vectors of properties, one for each particle type.
newtype PerParticleProp a =
  PerParticleProp {unPerParticleProp::M.Map ParticleType (Acc (A.Vector a))}
-- | 'PerParticleProp'' is an associated list of 'ParticleType's with
-- evaluated vectors of properties, one for each particle type.
newtype PerParticleProp' a =
  PerParticleProp' {unPerParticleProp'::M.Map ParticleType (A.Vector a)}

-- | Map a function onto each particle's attribute
perParticleMap::(Elt a, Elt b)=>(Exp a->Exp b)->PerParticleProp a->PerParticleProp b
perParticleMap f (PerParticleProp x) = PerParticleProp $ M.map (A.map f) x

-- | Join two 'PerParticleProp's of potentially different types
-- with a function, discarding any elements for types not in both
-- 'PerParticleProp's.
perParticleZipWith::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                    PerParticleProp a->PerParticleProp b->PerParticleProp c
perParticleZipWith f (PerParticleProp x) (PerParticleProp y) = PerParticleProp z
  where
    z = A.map (A.uncurry f) <$> M.intersectionWith A.zip x y

-- | Join two 'PerParticleProp's of the same type with a function,
-- carrying along elements for types not in both
-- 'PerParticleProp's.
perParticleZipWith'::Elt a=>(Exp a->Exp a->Exp a)->
                    PerParticleProp a->PerParticleProp a->PerParticleProp a
perParticleZipWith' f (PerParticleProp x) (PerParticleProp y) = PerParticleProp z
  where
    z' = A.map (A.uncurry f) <$> M.intersectionWith A.zip x y
    z = M.union z' $ M.union x y

-- | Add two 'PerParticleProp' 'Vec3's, discarding values for types
-- not in both 'PerParticleProp's.
plusp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
plusp3 = perParticleZipWith plus3
-- | Subtract a 'PerParticleProp' 'Vec3' from another, discarding
-- values for types not in both 'PerParticleProp's.
minusp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
minusp3 = perParticleZipWith minus3
-- | Multiply two 'PerParticleProp' 'Vec3's, discarding values for
-- types not in both 'PerParticleProp's.
timesp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
timesp3 = perParticleZipWith times3
-- | Divide a 'PerParticleProp' 'Vec3' by another, discarding values
-- for types not in both 'PerParticleProp's.
divp3::(Elt a, IsFloating a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
divp3 = perParticleZipWith div3

-- | Add two 'PerParticleProp' 'Vec3's, carrying along values for
-- types not in both 'PerParticleProp's.
plusp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
plusp3' = perParticleZipWith' plus3
-- | Subtract a 'PerParticleProp' 'Vec3' from another, carrying along
-- values for types not in both 'PerParticleProp's.
minusp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
minusp3' = perParticleZipWith' minus3
-- | Multiply two 'PerParticleProp' 'Vec3's, carrying along values for
-- types not in both 'PerParticleProp's.
timesp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
timesp3' = perParticleZipWith' times3
-- | Divide a 'PerParticleProp' 'Vec3' by another, carrying along values for
-- types not in both 'PerParticleProp's.
divp3'::(Elt a, IsFloating a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
divp3' = perParticleZipWith' div3

-- | Default Num instance of PerParticleProp only works for scalars
-- (not tuples, for example) and discards properties for types that
-- aren't present in both 'PerParticleProp's.
instance (Elt a, IsNum a)=>Num (PerParticleProp a) where
  fromInteger _ = undefined
  (+) = perParticleZipWith (+)
  (-) = perParticleZipWith (-)
  (*) = perParticleZipWith (*)
  negate = perParticleMap negate
  abs = perParticleMap abs
  signum = perParticleMap signum

instance (Elt a, IsNum a, IsFloating a)=>Fractional (PerParticleProp a) where
  fromRational _ = undefined
  recip = perParticleMap recip

-- | 'PerTypeProp' is an associated list of 'ParticleType's with
-- symbolic scalar quantities, one for each particle type.
newtype PerTypeProp a =
  PerTypeProp {unPerTypeProp::M.Map ParticleType (Acc (A.Scalar a))}
-- | 'PerTypeProp'' is an associated list of 'ParticleType's with
-- evaluated scalar quantities, one for each particle type.
newtype PerTypeProp' a =
  PerTypeProp' {unPerTypeProp'::M.Map ParticleType (A.Scalar a)}

-- | 'SingleProp' is a single symbolic scalar quantity.
newtype SingleProp a = SingleProp {unSingleProp::Acc (A.Scalar a)}
-- | 'SingleProp'' is a single evaluated scalar quantity.
newtype SingleProp' a = SingleProp' {unSingleProp'::A.Scalar a}

-- | Given a 'SingleProp' to broadcast and a 'PerParticleProp'
-- indicating the shapes to match, replicate the 'SingleProp' to the
-- same shape.
singleToParticleProp::Elt a=>SingleProp a->PerParticleProp a->PerParticleProp a
singleToParticleProp (SingleProp x) (PerParticleProp template) = PerParticleProp x'
  where
    x' = M.map (broadcast x) template
    broadcast from to = A.replicate (A.lift $ Z:.(A.size to)) from
