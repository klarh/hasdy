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

module Hasdy.Prop.Math where

import Data.Array.Accelerate as A

import Hasdy.Vectors
import Hasdy.Prop

-- | Add two 'PerParticleProp' 'Vec3's, discarding values for types
-- not in both 'PerParticleProp's.
plusp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
        PerParticleProp (Vec3' a)
plusp3 = perParticleZipWith plus3
-- | Subtract a 'PerParticleProp' 'Vec3' from another, discarding
-- values for types not in both 'PerParticleProp's.
minusp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
         PerParticleProp (Vec3' a)
minusp3 = perParticleZipWith minus3
-- | Multiply two 'PerParticleProp' 'Vec3's, discarding values for
-- types not in both 'PerParticleProp's.
timesp3::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
         PerParticleProp (Vec3' a)
timesp3 = perParticleZipWith times3
-- | Divide a 'PerParticleProp' 'Vec3' by another, discarding values
-- for types not in both 'PerParticleProp's.
divp3::(Elt a, IsFloating a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
       PerParticleProp (Vec3' a)
divp3 = perParticleZipWith div3

-- | Add two 'PerParticleProp' 'Vec3's, carrying along values for
-- types not in both 'PerParticleProp's.
plusp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
         PerParticleProp (Vec3' a)
plusp3' = perParticleZipWith' plus3
-- | Subtract a 'PerParticleProp' 'Vec3' from another, carrying along
-- values for types not in both 'PerParticleProp's.
minusp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
          PerParticleProp (Vec3' a)
minusp3' = perParticleZipWith' minus3
-- | Multiply two 'PerParticleProp' 'Vec3's, carrying along values for
-- types not in both 'PerParticleProp's.
timesp3'::(Elt a, IsNum a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
          PerParticleProp (Vec3' a)
timesp3' = perParticleZipWith' times3
-- | Divide a 'PerParticleProp' 'Vec3' by another, carrying along values for
-- types not in both 'PerParticleProp's.
divp3'::(Elt a, IsFloating a)=>PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)->
        PerParticleProp (Vec3' a)
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
