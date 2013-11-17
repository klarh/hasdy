import Control.Applicative
import Control.Monad
import Data.Array.Accelerate as A
import Data.Map as M
import Data.Monoid
import Data.Text.IO (writeFile, appendFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment
import System.IO (FilePath(..))
import System.Random (randomRs, mkStdGen)
import Data.NumInstances
import GHC.Float (double2Float)

import Hasdy.Types
import Hasdy.Prop
import Hasdy.Neighbor.Slow
import Hasdy.Integrate.Leapfrog
import Hasdy.Potentials.LJ
import Hasdy.Potentials.Sigmoidal
import Hasdy.Spatial
import Hasdy.Vectors
import Hasdy.Dump.Pos

import Data.Array.Accelerate.CUDA

-- global constants
scale = 2
n = 16
v0 = 1e-1
box = A.constant box'
box' = scale3' scale . pure3' $ Prelude.fromIntegral n
lj = LJ (unit 1) (unit 1) (unit 3) :: LJ Float
sig = Sigmoidal (unit 1) (unit 1) (unit 5) (unit 2) :: Sigmoidal Float
dt = constToSingleProp 0.005
typ = ParticleType 0

-- | a single timestep in terms of 'PerParticleProp's
timestep::(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep (pos, vel) = leapfrog dt masses forces (pos', vel)
  where
    pos' = perParticleMap (wrapBox box id) pos
    forces = foldNeighbors (makeAbsolute . wrapBox box $ sigmoidalForce sig) plus3 (A.constant (0, 0, 0)) typ typ pos'
    masses = singleToParticleProp (constToSingleProp 1) pos
    typ = ParticleType 0

-- | several timesteps + dumping to file
timestep'::(PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))->
           IO (PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))
timestep' (positions, velocities) = do
  let posvelIn = bundlePerParticle' typ posIn velocities
      posIn = bundlePerParticle' typ () positions
      posvelOut = timestep'' posvelIn
      (pos'', velocities'') = unBundlePerParticle' typ posvelOut
      (_, positions'') = unBundlePerParticle' typ pos''
  Data.Text.IO.appendFile "dump.pos" $ toStrict . toLazyText . posFrame box' $ unPerParticleProp' positions'' M.! typ
  return (positions'', velocities'')

-- | a group of timesteps glued together under accelerate's run1
timestep'' = run1 accTimestep
  where
    accTimestep posvelIn = bundlePerParticle typ posOut velocities'
      where
        (posIn, velocities) = unBundlePerParticle typ posvelIn
        (_, positions) = unBundlePerParticle typ posIn
        (positions', velocities') = iterate timestep (positions, velocities) Prelude.!! 10
                                    :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
        posOut = bundlePerParticle typ (A.use ()) positions'

-- | run n groups of timesteps
multitimestep n (pos, vel) = do
  (pos', vel') <- timestep' (pos, vel)
  if n == 0
    then return (pos', vel')
    else multitimestep (n-1) (pos', vel')

main = do
  let grid idx = r `minus3` center
        where
          r = scale3 (A.constant scale) $ A.lift (A.fromIntegral x, A.fromIntegral y, A.fromIntegral z)
          (Z:.x:.y:.z) = A.unlift idx
          center = pure3 . A.constant $ (Prelude.fromIntegral n / 2 :: Float)
      positions' = run . A.flatten $ A.generate (A.constant $ (Z:.n:.n:.n) :: Exp DIM3) grid
      positions = PerParticleProp $ M.fromList [(ParticleType 0, use positions')]
      triplify (x:y:z:rest) = (x, y, z):(triplify rest)
      velocities' = A.fromList (Z:.(n*n*n)) . triplify $ randomRs (negate v0, v0) (mkStdGen 5336) :: Vector (Vec3' Float)
      velocities = PerParticleProp $ M.fromList [(ParticleType 0, use velocities')] :: PerParticleProp (Vec3' Float)
  (n':_) <- getArgs
  let n = read n'::Int
  multitimestep n (runPerParticle run positions, runPerParticle run velocities)
  return ()
