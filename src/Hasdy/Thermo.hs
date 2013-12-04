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

module Hasdy.Thermo where

import Data.Array.Accelerate as A

import Hasdy.Prop
import Hasdy.Vectors

temperature::(Elt a, IsFloating a)=>PerParticleProp a->PerParticleProp (Vec3' a)->SingleProp a
temperature mass velocity = SingleProp . unit $ 2/3*totalEk/totalN
  where
    totalEk = the . unSingleProp $ totalKineticEnergy mass velocity
    totalN = A.fromIntegral . the . unSingleProp . reducePerType (+) 0 . countPerParticle $ mass

kineticEnergy::(Elt a, IsFloating a)=>PerParticleProp a->PerParticleProp (Vec3' a)->PerParticleProp a
kineticEnergy mass velocity = perParticleZipWith (\m vsq -> 0.5*m*vsq) mass velsq
  where
    velsq = perParticleZipWith dot3 velocity velocity

totalKineticEnergy::(Elt a, IsFloating a)=>PerParticleProp a->PerParticleProp (Vec3' a)->SingleProp a
totalKineticEnergy mass velocity = reducePerType (+) 0 . reducePerParticle (+) 0 $ kineticEnergy mass velocity
