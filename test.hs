
import Data.Array.Accelerate as A
import Data.Map as M
import Data.NumInstances

import Hasdy.Types
import Hasdy.Prop
import Hasdy.Neighbor.Slow
import Hasdy.Integrate.Leapfrog
import Hasdy.Potentials.LJ
import Hasdy.Spatial
import Hasdy.Vectors
import Hasdy.Dump.Pos

import Data.Array.Accelerate.Interpreter

lj = LJ (unit 1) (unit 1) (unit 3) :: LJ Float

timestep::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep (pos, vel) = leapfrog (constToSingleProp 0.005) masses forces (pos, vel)
  where
    masses = singleToParticleProp (constToSingleProp 1) pos
    forces = foldNeighbors (makeRelative $ ljForce lj) plus3 (A.constant (0, 0, 0)) typ typ pos
    typ = ParticleType 0

main = do
  let dr = run . unit $ A.constant (1, 0, 0)
      ljf r = unit $ ljForce lj (the r)
      ljp r = unit $ ljPotential lj (the r)
      f = run1 ljf dr
      u = run1 ljp dr
  print (f, u)
