module Hasdy.Spatial where

import Data.Array.Accelerate as A

import Hasdy.Vectors (Vec3, minus3, zipWith3)

-- | Transform an operation that transforms a function of a relative
-- 'Vec3' into one that works on two absolute 'Vec3's.
makeAbsolute::(Elt a, IsNum a)=>(Vec3 a->Vec3 b)->Vec3 a->Vec3 a->Vec3 b
makeAbsolute f = \x y -> f (y `minus3` x)

-- | Apply periodic boundary conditions by wrapping a relative
-- position to be inside a box of a given size.
wrapBox::(Elt a, Elt b, IsNum a, IsFloating a)=>Vec3 a->(Vec3 a->Vec3 b)->Vec3 a->Vec3 b
wrapBox box f = \r -> f (Hasdy.Vectors.zipWith3 wrap box r)
  where
    wrap box x = (x >* (half box))? (x - box, (x <* (negate . half $ box))? (x + box, x))
    half box = 0.5*box
