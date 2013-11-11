module Hasdy.Integrate.Leapfrog where

import Data.Array.Accelerate as A
import Hasdy.Prop (PerParticleProp)
import Hasdy.Types (Vec3')

leapfrog::(Elt r, IsFloating r, IsNum r)=>SingleProp r->PerParticleProp r->
          PerParticleProp (Vec3' r)->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))->
          (PerParticleProp (Vec3' r), PerParticleProp (Vec3' r))
leapfrog dt masses forces (positions, velocities) = (positions', velocities')
  where
    dt' = singleToParticleProp dt
    adt = zipParticlePropWith scale3 (dt'/masses) forces
    velocities' = velocities + adt
    positions' = positions + zipParticlePropWith scale3 dt' velocities'
