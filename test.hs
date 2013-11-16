
import Control.Monad
import Data.Array.Accelerate as A
import Data.Map as M
import Data.Monoid
import Data.Text.IO (writeFile, appendFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment
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
dt = constToSingleProp 0.005

timestep::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep (pos, vel) = leapfrog dt masses forces (pos, vel)
  where
    forces = foldNeighbors (makeRelative $ ljForce lj) plus3 (A.constant (0, 0, 0)) typ typ pos
    masses = singleToParticleProp (constToSingleProp 1) pos
    typ = ParticleType 0

timestep' (pos, vel) = do
  let (pos', vel') = iterate timestep (pos, vel) Prelude.!! 10 :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
      positionsA = run (unPerParticleProp pos' M.! ParticleType 0) :: Vector (Vec3' Float)
  Data.Text.IO.appendFile "dump.pos" $ toStrict . toLazyText . posFrame $ positionsA
  return (pos', vel')

multitimestep n (pos, vel) = do
  (pos', vel') <- timestep' (pos, vel)
  if n == 0
    then return (pos', vel')
    else multitimestep (n-1) (pos', vel')

-- timestep'::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->IO (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
-- timestep' (positions, velocities) = do
--   let posvel' = run1 timestep'' posvel
--       posvel = bundlePerParticle typ pos velocities
--       pos = bundlePerParticle typ (A.use ()) positions
--       (pos', velocities'') = A.unlift posvel'
--       ((), positions'') = A.unlift pos'
--       positions' = M.fromList [(typ, positions'')]
--       velocities' = M.fromList [(typ, velocities'')]
--       typ = ParticleType 0
--   Data.Text.IO.appendFile "dump.pos" $ toStrict . toLazyText . posFrame $ positions'
--   return (positions', velocities')

-- timestep''::Acc (((), Vector (Vec3' Float)), Vector (Vec3' Float))->Acc (((), Vector (Vec3' Float)), Vector (Vec3' Float))
-- timestep'' posvel = bundlePerParticle typ pos' velocities'
--   where
--     (positions', velocities') = iterate timestep (positions, velocities) Prelude.!! 10
--     pos' = bundlePerParticle typ (A.use ()) positions'
-- --    pos' = unPerParticleProp positions M.! typ
--     (positions, velocities) = A.unlift posvel
--     typ = ParticleType 0

-- timestep''::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
-- timestep'' (positions, velocities) = unpack $ A.run1 timestep' (pack positions)
--   where
--     pack vel = bundlePerParticle typ pos' vel
--     pos' = unPerParticleProp velocities M.! typ
--     typ = ParticleType 0
--     unpack arrs = let (pos, velocities')

main = do
  let grid idx = A.lift (scale*(A.fromIntegral x), scale*(A.fromIntegral y), scale*(A.fromIntegral z))
        where
          (Z:.x:.y:.z) = A.unlift idx
          scale = 1.7
      n = 16
      positions' = run . A.flatten $ A.generate (A.constant $ (Z:.n:.n:.n) :: Exp DIM3) grid
      positions = PerParticleProp $ M.fromList [(ParticleType 0, use positions')]
      velocities = perParticleMap (scale3 0) positions
  [n'] <- getArgs
  let n = read n'::Int
  multitimestep n (positions, velocities)
  return ()
