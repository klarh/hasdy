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

module Hasdy.Thermostats where

import Data.Array.Accelerate as A

import Hasdy.Prop
import Hasdy.Thermo (temperature)
import Hasdy.Vectors

-- | Rescale the velocity of the system to a reference value
rescale::(Elt a, IsFloating a)=>SingleProp a->PerParticleProp a->
         PerParticleProp (Vec3' a)->PerParticleProp (Vec3' a)
rescale tRef mass velocity = perParticleMap (scale3 lambda) velocity
  where
    temp = the . unSingleProp $ temperature mass velocity
    lambda = sqrt $ (the . unSingleProp $ tRef)/temp
