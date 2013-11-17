
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

scale = 1.5
n = 5
box = A.constant box'
box' = scale3' scale . pure3' $ Prelude.fromIntegral n
lj = LJ (unit 1) (unit 1) (unit 3) :: LJ Float
dt = constToSingleProp 0.005
typ = ParticleType 0

timestep::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep (pos, vel) = leapfrog dt masses forces (pos', vel)
  where
    pos' = perParticleMap (wrapBox box id) pos
    forces = foldNeighbors (makeAbsolute . wrapBox box $ ljForce lj) plus3 (A.constant (0, 0, 0)) typ typ pos'
    masses = singleToParticleProp (constToSingleProp 1) pos
    typ = ParticleType 0

-- timestep' (pos, vel) = do
--   let (pos', vel') = iterate timestep (pos, vel) Prelude.!! 10 :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
--       positionsA' = unPerParticleProp pos' M.! ParticleType 0 :: Acc (Vector (Vec3' Float))
--       positionsA = run positionsA'
--   Data.Text.IO.appendFile "dump.pos" $ toStrict . toLazyText . posFrame $ positionsA
-- --  print positionsA'
--   return (pos', vel')

multitimestep n (pos, vel) = do
  (pos', vel') <- timestep' (pos, vel)
  if n == 0
    then return (pos', vel')
    else multitimestep (n-1) (pos', vel')

timestep'::(PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))->IO (PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))
timestep' (positions, velocities) = do
  let accTimestep posvelIn = bundlePerParticle typ posOut velocities'
        where
          (posIn, velocities) = unBundlePerParticle typ posvelIn
          (_, positions) = unBundlePerParticle typ posIn
          (positions', velocities') = iterate timestep (positions, velocities) Prelude.!! 10 :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
          posOut = bundlePerParticle typ (A.use ()) positions'
      posvelIn = bundlePerParticle' typ posIn velocities
      posIn = bundlePerParticle' typ () positions
      posvelOut = run1 accTimestep posvelIn
      (pos'', velocities'') = unBundlePerParticle' typ posvelOut
      (_, positions'') = unBundlePerParticle' typ pos''
  Data.Text.IO.appendFile "dump.pos" $ toStrict . toLazyText . posFrame box' $ unPerParticleProp' positions'' M.! typ
  return (positions'', velocities'')

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
  let grid idx = r `minus3` offset
        where
          r = scale3 (A.constant scale) $ A.lift (A.fromIntegral x, A.fromIntegral y, A.fromIntegral z)
          (Z:.x:.y:.z) = A.unlift idx
          offset = pure3 . A.constant $ (Prelude.fromIntegral n / 2 :: Float)
      positions' = run . A.flatten $ A.generate (A.constant $ (Z:.n:.n:.n) :: Exp DIM3) grid
      positions = PerParticleProp $ M.fromList [(ParticleType 0, use positions')]
      velocities = perParticleMap (scale3 1e-1) positions
  (n':_) <- getArgs
  let n = read n'::Int
  multitimestep n (runPerParticle run positions, runPerParticle run velocities)
  return ()
