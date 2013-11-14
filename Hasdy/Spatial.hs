module Hasdy.Spatial where

import Data.Array.Accelerate as A

import Hasdy.Vectors (Vec3, minus3)

-- | Transform an operation that works on a relative 'Vec3' into one
-- that works on two absolute 'Vec3's.
makeRelative::(Elt a, IsNum a)=>(Vec3 a->Vec3 b)->Vec3 a->Vec3 a->Vec3 b
makeRelative f = \x y -> f (y `minus3` x)
