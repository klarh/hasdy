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

module Hasdy.Spatial where

import Data.Array.Accelerate as A

import Hasdy.Vectors (Vec3, dot3, minus3, zipWith3)

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

cutoff::(Elt a, Elt b, IsNum a)=>Exp b->Exp a->(Vec3 a->Exp b)->Vec3 a->Exp b
cutoff x0 rmaxsq f r = dot3 r r <* rmaxsq? (f r, x0)
