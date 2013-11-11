
import Data.Array.Accelerate as A
import Data.Map as M
import Data.NumInstances

import Hasdy.Types
import Hasdy.Prop
import Hasdy.Neighbor.Slow
import Hasdy.Potentials.LJ

import Data.Array.Accelerate.Interpreter

main = do
  let dr = run . unit $ A.constant (2.0**(1.0/6), 0, 0)
      lj = LJ (unit 1) (unit 1) (unit 3) :: LJ Float
      ljf r = unit $ ljForce lj (the r)
      ljp r = unit $ ljPotential lj (the r)
      f = run1 ljf dr
      u = run1 ljp dr
  print (f, u)
