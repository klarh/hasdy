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

import Control.Applicative
import Control.Monad
import Data.Array.Accelerate as A
import Data.Map as M
import Data.Monoid
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment
import System.IO (FilePath(..), IOMode(..), Handle(..), openFile, hFlush)
import System.Random (randomRs, mkStdGen)
import Data.NumInstances
import GHC.Float (double2Float)

import Data.Array.Accelerate.HasdyContrib as HC
import Hasdy.Types
import Hasdy.Prop
import Hasdy.Neighbor.Slow as Slow
import Hasdy.Integrate.Leapfrog
import Hasdy.Potentials.LJ
import Hasdy.Potentials.Sigmoidal
import Hasdy.Spatial
import Hasdy.Vectors
import Hasdy.Dump.Pos
import Hasdy.Thermo
import Hasdy.Thermostats
import Hasdy.Utils
import Hasdy.Utils.Sort
import Hasdy.Neighbor.Fast as Fast

import Data.Array.Accelerate.CUDA
--import Data.Array.Accelerate.Interpreter

-- global constants
scale = 3.2
n = 6
v0 = 1e-2
box = A.constant box'
box' = scale3' scale . pure3' $ Prelude.fromIntegral n
cell = constToSingleProp . pure3' $ 3.1
lj = LJ (unit 1) (unit 1) :: LJ Float
sig = Sigmoidal (unit 1) (unit 1) (unit 3) :: Sigmoidal Float
dt = constToSingleProp 0.005
typ = ParticleType 0

-- | a single timestep in terms of 'PerParticleProp's
timestep::NList->(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep nlist (pos, vel) = leapfrog dt masses forces (pos', vel')
  where
    pos' = perParticleMap (wrapBox box id) pos
    vel' = rescale (constToSingleProp 1) masses vel
--    forces = Slow.foldNeighbors (makeAbsolute . wrapBox box $ ljForce lj) plus3 (A.constant (0, 0, 0)) typ typ pos'
    forces = Fast.foldNeighbors (makeAbsolute . wrapBox box $ ljForce lj) plus3 (A.constant (0, 0, 0)) nlist typ typ pos' pos'
    masses = singleToParticleProp (constToSingleProp 1) pos
    typ = ParticleType 0

-- | several timesteps + dumping to file
timestep'::Handle->(PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))->
           IO (PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))
timestep' handle (positions, velocities) = do
  let posvelIn = bundlePerParticle' typ posIn velocities
      posIn = bundlePerParticle' typ () positions
      posvelOut = timestep'' posvelIn
      (pos'', velocities'') = unBundlePerParticle' typ posvelOut
      (_, positions'') = unBundlePerParticle' typ pos''

  Data.Text.Lazy.IO.hPutStr handle $ toLazyText . posFrame box' $ unPerParticleProp' positions'' M.! typ
  hFlush handle
  return (positions'', velocities'')

-- | a group of timesteps glued together under accelerate's run1
timestep'' = run1 accTimestep
  where
    accTimestep posvelIn = bundlePerParticle typ posOut velocities''
      where
        (posIn, velocities) = unBundlePerParticle typ posvelIn
        (_, positions) = unBundlePerParticle typ posIn
        (nlist, oldIdx) = buildNList True cell (constToSingleProp box') (perParticleMap (wrapBox box id) positions)
        positions' = gatherPerParticle oldIdx positions
        velocities' = gatherPerParticle oldIdx velocities
        (positions'', velocities'') = Prelude.iterate (timestep nlist) (positions', velocities') Prelude.!! 2
                                    :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
        posOut = bundlePerParticle typ (A.use ()) positions''

-- | run n groups of timesteps
multitimestep n handle (pos, vel) = do
  (pos', vel') <- timestep' handle (pos, vel)
  if n <= 0
    then return (pos', vel')
    else multitimestep (n-1) handle (pos', vel')

main = do
  let grid idx = r `minus3` center
        where
          r = scale3 (A.constant scale) $ A.lift (A.fromIntegral x, A.fromIntegral y, A.fromIntegral z)
          (Z:.x:.y:.z) = A.unlift idx
          center = scale3 0.5 box
      positions' = run . A.flatten $ A.generate (A.constant $ (Z:.n:.n:.n) :: Exp DIM3) grid
      positions = PerParticleProp $ M.fromList [(ParticleType 0, use positions')]
      triplify (x:y:z:rest) = (x, y, z):(triplify rest)
      velocities' = A.fromList (Z:.(n*n*n)) . triplify $ randomRs (negate v0, v0) (mkStdGen 5337) :: Vector (Vec3' Float)
      velocities = PerParticleProp $ M.fromList [(ParticleType 0, use velocities')] :: PerParticleProp (Vec3' Float)
      vel' = rescale (constToSingleProp 1) masses velocities
      masses = singleToParticleProp (constToSingleProp 1) positions
  (n':_) <- getArgs
  handle <- openFile "dump.pos" WriteMode
  let n = read n'::Int
  (positions, velocities) <- multitimestep n handle (runPerParticle run positions, runPerParticle run velocities)

  let --positions' = A.use . A.fromList (Z:.27) $ [(x, y, z) | x <- [negate 0.5, 0.25, 1.5], y <- [negate 0.5, 0.25, 1.25], z <- [negate 0.5, 0.5, 1.5]]
      --positions = A.map (plus3 $ pure3 (negate 4)) positions'
      --positions = A.use . A.fromList (Z:.2) $ [(0.01, 0.01, 0.01),  (4.5, 0, 0.5)] :: Acc (A.Vector (Vec3' Float))
      --cell = constToSingleProp (0.999, 0.999, 0.999) :: SingleProp (Vec3' Float)
      --box = constToSingleProp (10, 10, 10) :: SingleProp (Vec3' Float)
      ((NList' idxI idxJ segments), oldIdx) = buildNList' True cell (SingleProp . unit $ box) (use . (flip (M.!) $ typ) . unPerParticleProp' $ positions)
--      blah = buildNList'' True cell box positions
--  print . run $ positions
  print . run $ A.zip idxI idxJ
  print . run $ idxI
  print . run $ idxJ
  print . run $ segments
  print . run $ A.sum segments
  print . run $ oldIdx
--  print . run $ A.gather oldIdx positions
--  print . run $ blah
--  print . run $ A.sum blah
  -- putStrLn . Prelude.take 80 . Prelude.repeat $ '-'
  -- let positions = A.use . A.fromList (Z:.2) $ [(-3.5,-4.5,-2.5),(-3.5,-4.0,-4.5)]
  --     ((NList' idxI idxJ segments), oldIdx) = buildNList' True cell box positions
  -- print . run $ buildNList'' True cell box positions
  -- print . run $ idxI
  -- print . run $ A.sum segments
  -- print . run $ segments

  return ()
