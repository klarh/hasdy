module Hasdy.Integrate.Leapfrog where

import Data.Array.Accelerate as A
import Hasdy.Prop (PerParticleProp, SingleProp, singleToParticleProp, perParticleZipWith)
import Hasdy.Vectors (Vec3', scale3, plus3)

leapfrog::(Elt r, IsFloating r, IsNum r)=>SingleProp r->PerParticleProp r->
          PerParticleProp (Vec3' r)->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))
leapfrog dt masses forces (positions, velocities) = (positions', velocities')
  where
    dt' = singleToParticleProp dt masses
    adt = perParticleZipWith scale3 (dt'/masses) forces
    velocities' = perParticleZipWith plus3 velocities adt
    positions' = perParticleZipWith plus3 positions $ perParticleZipWith scale3 dt' velocities'
