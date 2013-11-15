
import Data.Array.Accelerate as A
import Data.Map as M
import Data.Text.IO (writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import System.IO (FilePath(..))
import Data.NumInstances

import Hasdy.Types
import Hasdy.Prop
import Hasdy.Neighbor.Slow
import Hasdy.Integrate.Leapfrog
import Hasdy.Potentials.LJ
import Hasdy.Spatial
import Hasdy.Vectors
import Hasdy.Dump.Pos

-- import Data.Array.Accelerate.Interpreter
-- import Control.Monad.Par.Meta.AccSMP
-- import Control.Monad.Par.Accelerate
import Data.Array.Accelerate.CUDA

lj = LJ (unit 1) (unit 1) (unit 3) :: LJ Float

timestep::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep (pos, vel) = leapfrog (constToSingleProp 0.005) masses forces (pos, vel)
  where
    masses = singleToParticleProp (constToSingleProp 1) pos
    forces = foldNeighbors (makeRelative $ ljForce lj) plus3 (A.constant (0, 0, 0)) typ typ pos
    typ = ParticleType 0

main = do
  let dr = run . unit $ A.constant (0.5, 1, 0)
      ljf r = unit $ ljForce lj (the r)
      ljp r = unit $ ljPotential lj (the r)
      f = run1 ljf dr
      u = run1 ljp dr
  print (f, u)
  let grid idx = A.lift (scale*(A.fromIntegral x), scale*(A.fromIntegral y), scale*(A.fromIntegral z))
        where
          (Z:.x:.y:.z) = A.unlift idx
          scale = 1.7
      positions = run . A.flatten $ A.generate (A.constant $ (Z:.4:.4:.4) :: Exp DIM3) grid
      positions' = PerParticleProp $ M.fromList [(ParticleType 0, use positions)]
      velocities = perParticleMap (scale3 0) positions'
  Data.Text.IO.writeFile "init.pos" $ toStrict . toLazyText . posFrame $ positions
  let (positions'', velocities'') = iterate timestep (positions', velocities) Prelude.!! 10 :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
  let positions = run (unPerParticleProp positions'' M.! ParticleType 0) :: Vector (Vec3' Float)
  Data.Text.IO.writeFile "final.pos" $ toStrict . toLazyText . posFrame $ positions
