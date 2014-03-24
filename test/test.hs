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
import Data.Text.Lazy.IO (hPutStr)
import Data.Text.Lazy.Builder (toLazyText)
import System.Environment
import System.IO (FilePath(..), IOMode(..), Handle(..), openFile, hFlush, hClose)
import System.Random (randomRs, mkStdGen)

import Data.Array.Accelerate.HasdyContrib as HC
import Hasdy.Types
import Hasdy.Prop
import Hasdy.Prop.Bundle
import Hasdy.Neighbor.Slow as Slow
import Hasdy.Integrate.Leapfrog
import Hasdy.Integrate.VelVerlet
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
scale = 1.5
n = 8
v0 = 1e-1
box = A.constant box'
box' = scale3' scale . pure3' $ Prelude.fromIntegral n
rbuf = constToSingleProp 3
cellR = constToSingleProp 6
lj = LJ (unit 1) (unit 1) :: LJ Float
sig = Sigmoidal (unit 1) (unit 1) (unit 3) :: Sigmoidal Float
dt = constToSingleProp 0.005
typ = ParticleType 0

state = usePP typ <. newPP <. useNList typ <. newNList <. -- neighbor list + old positions
        usePP typ <. newPP <. -- accelerations
        usePP typ <. newPP <. -- velocities
        usePP typ <. newPP -- positions

-- | a single timestep in terms of 'PerParticleProp's
timestep::NList->(PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))->
          (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
timestep nlist (pos, vel, acc) = velVerlet dt masses forceCalc (pos', vel', acc)
  where
    pos' = perParticleMap (wrapBox box id) pos
    vel' = rescale (constToSingleProp 1) masses vel
    force = makeAbsolute . wrapBox box . cutoff (A.constant (0, 0, 0)) (3**2) $ ljForce lj
    forceCalc p = Fast.foldNeighbors force plus3 (A.constant (0, 0, 0)) nlist typ typ p p
    masses = singleToParticleProp (constToSingleProp 1) pos

-- | a group of timesteps glued together under accelerate's run1; also
-- rebuilds neighbor lists if necessary
runTimesteps = runBLens run1 (canal1 state . liftAccs $ f) state state
  where
    f = Prelude.foldr1 (>->) . Prelude.take 10 . Prelude.repeat $ unliftAccs . bridge1 state $ accTimestep
    accTimestep bundIn = bundOut
      where
        ((((((), positions), velocities), accelerations), nlist), oldPositions) = bundleProps bundIn

        (rebuilt, nlist', oldIdx) = maybeRebuildNList True cellR rbuf (constToSingleProp box') oldPositions positions nlist
        positions' = maybeGatherPerParticle rebuilt oldIdx positions
        velocities' = maybeGatherPerParticle rebuilt oldIdx velocities
        accelerations' = maybeGatherPerParticle rebuilt oldIdx accelerations
        oldPositions' = maybeGatherPerParticle rebuilt oldIdx positions

        (positions'', velocities'', accelerations'') = Prelude.iterate (timestep nlist') (positions', velocities', accelerations') Prelude.!! 10
                                    :: (PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float), PerParticleProp (Vec3' Float))
        bundOut = Bundle ((((((), positions''), velocities''), accelerations''), nlist'), oldPositions') (A.use ())

-- | several timesteps + dumping to file
-- dumpTimesteps::Handle->(PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))->
--            IO (PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float), PerParticleProp' (Vec3' Float))
dumpTimesteps handle (positions, velocities, accelerations, nlist, oldPositions) = do
  let posvelaccIn = Bundle ((((((), positions), velocities), accelerations), nlist), oldPositions) ()
      ((((((), positions''), velocities''), accelerations''), nlist''), oldPositions'') =
        bundleProps . runTimesteps $ posvelaccIn

  Data.Text.Lazy.IO.hPutStr handle $ toLazyText . posFrame box' $ unPerParticleProp' positions'' M.! typ
  hFlush handle
  return (positions'', velocities'', accelerations'', nlist'', oldPositions'')

-- | run n groups of timesteps
multitimestep n handle (pos, vel, acc, nl, oldP) = do
  (pos', vel', acc', nl', oldP') <- dumpTimesteps handle (pos, vel, acc, nl, oldP)
  if n <= 0
    then return (pos, vel, acc, nl, oldP)
    else multitimestep (n-1) handle (pos', vel', acc', nl', oldP')

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
      accelerations = perParticleMap (scale3 0) velocities
      vel' = rescale (constToSingleProp 1e-3) masses velocities
      masses = singleToParticleProp (constToSingleProp 1) positions
  (n':_) <- getArgs
  handle <- openFile "dump.pos" WriteMode
  let n = read n'::Int
      positions' = runPerParticle run positions
      velocities' = runPerParticle run velocities
      accelerations' = runPerParticle run accelerations
      emptyIdx = A.fromList (Z:.0) [] :: A.Vector Int
      nlist0 = NList' . M.fromList $ [(typ, SNList' emptyIdx emptyIdx emptyIdx)]
      initState = (positions', velocities', accelerations', nlist0, accelerations')
  (positions, velocities, accelerations, _, _) <- multitimestep n handle initState
  hClose handle

  return ()
