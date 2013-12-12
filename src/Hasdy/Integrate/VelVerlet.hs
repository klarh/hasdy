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

module Hasdy.Integrate.VelVerlet where

import Data.Array.Accelerate as A
import Hasdy.Prop (PerParticleProp, SingleProp, unSingleProp, singleToParticleProp, perParticleMap,
                   perParticleZipWith)
import Hasdy.Prop.Math (plusp3, plusp3')
import Hasdy.Vectors (Vec3', scale3)

velVerlet::(Elt r, IsFloating r, IsNum r)=>SingleProp r->PerParticleProp r->(PerParticleProp (Vec3' r)->PerParticleProp (Vec3' r))->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))
velVerlet dt masses forceComp (positions, velocities, accelerations) = (positions', velocities', accelerations')
  where
    dt' = the . unSingleProp $ dt
    positions' = positions `plusp3'` perParticleMap (scale3 dt') velocities `plusp3'`
                 perParticleMap (scale3 ((A.constant 0.5)*dt'*dt')) accelerations
    forces = forceComp positions'
    accelerations' = perParticleZipWith scale3 (perParticleMap recip masses) forces
    meanAccelerations = perParticleMap (scale3 0.5) $ accelerations `plusp3` accelerations'
    velocities' = velocities `plusp3'` perParticleMap (scale3 dt') meanAccelerations
