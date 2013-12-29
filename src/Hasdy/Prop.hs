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

{-# LANGUAGE FlexibleContexts #-}
module Hasdy.Prop where

import Control.Applicative ((<$>))
import Data.Array.Accelerate as A
import Data.Map as M

import Hasdy.Types (ParticleType)
import Hasdy.Vectors (Vec3'(..), plus3, minus3, times3, div3)

-- | 'PerParticleProp' is an associated list of 'ParticleType's with
-- symbolic vectors of properties, one for each particle type.
newtype PerParticleProp a =
  PerParticleProp {unPerParticleProp::M.Map ParticleType (Acc (A.Vector a))}
-- | 'PerParticleProp'' is an associated list of 'ParticleType's with
-- evaluated vectors of properties, one for each particle type.
newtype PerParticleProp' a =
  PerParticleProp' {unPerParticleProp'::M.Map ParticleType (A.Vector a)}

-- | Transform a set of 'PerParticleProp''s into 'PerParticleProp's
usePerParticle::Elt a=>PerParticleProp' a->PerParticleProp a
usePerParticle (PerParticleProp' m) = PerParticleProp $ A.use <$> m

-- | Evaluate a symbolic 'PerParticleProp' given an evaluation function
runPerParticle::(Arrays (Vector a), Elt a)=>(Acc (Vector a)->Vector a)->PerParticleProp a->PerParticleProp' a
runPerParticle run (PerParticleProp m) = PerParticleProp' $ run <$> m

-- | Map a function onto each particle's attribute
perParticleMap::(Elt a, Elt b)=>(Exp a->Exp b)->PerParticleProp a->PerParticleProp b
perParticleMap f (PerParticleProp x) = PerParticleProp $ M.map (A.map f) x

-- | Join two 'PerParticleProp's of potentially different types
-- with a function, discarding any elements for types not in both
-- 'PerParticleProp's.
perParticleZipWith::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
                    PerParticleProp a->PerParticleProp b->PerParticleProp c
perParticleZipWith f (PerParticleProp x) (PerParticleProp y) = PerParticleProp z
  where
    z = A.map (A.uncurry f) <$> M.intersectionWith A.zip x y

-- | Join two 'PerParticleProp's of the same type with a function,
-- carrying along elements for types not in both
-- 'PerParticleProp's.
perParticleZipWith'::Elt a=>(Exp a->Exp a->Exp a)->
                    PerParticleProp a->PerParticleProp a->PerParticleProp a
perParticleZipWith' f (PerParticleProp x) (PerParticleProp y) = PerParticleProp z
  where
    z' = A.map (A.uncurry f) <$> M.intersectionWith A.zip x y
    z = M.union z' $ M.union x y

-- | Rearranges the elements of a 'PerParticleProp' according to an
-- index mapping.
gatherPerParticle::(Elt a)=>PerParticleProp Int->PerParticleProp a->PerParticleProp a
gatherPerParticle idx = PerParticleProp . M.intersectionWith A.gather (unPerParticleProp idx) . unPerParticleProp

-- | Reduce a 'PerParticleProp' into a 'PerTypeProp' using a monoidal function
reducePerParticle::Elt a=>(Exp a->Exp a->Exp a)->Exp a->PerParticleProp a->PerTypeProp a
reducePerParticle f x0 = PerTypeProp . M.map (A.fold f x0) . unPerParticleProp

-- | Count the number of particles of each type
countPerParticle::Elt a=>PerParticleProp a->PerTypeProp Int
countPerParticle = PerTypeProp . M.map (A.unit . A.size) . unPerParticleProp

-- | 'PerTypeProp' is an associated list of 'ParticleType's with
-- symbolic scalar quantities, one for each particle type.
newtype PerTypeProp a =
  PerTypeProp {unPerTypeProp::M.Map ParticleType (Acc (A.Scalar a))}
-- | 'PerTypeProp'' is an associated list of 'ParticleType's with
-- evaluated scalar quantities, one for each particle type.
newtype PerTypeProp' a =
  PerTypeProp' {unPerTypeProp'::M.Map ParticleType (A.Scalar a)}

reducePerType::Elt a=>(Exp a->Exp a->Exp a)->Exp a->PerTypeProp a->SingleProp a
reducePerType f x0 = SingleProp . unit . M.foldr (f . the) x0 . unPerTypeProp

-- | 'SingleProp' is a single symbolic scalar quantity.
newtype SingleProp a = SingleProp {unSingleProp::Acc (A.Scalar a)}
-- | 'SingleProp'' is a single evaluated scalar quantity.
newtype SingleProp' a = SingleProp' {unSingleProp'::A.Scalar a}

-- | Given a 'SingleProp' to broadcast and a 'PerParticleProp'
-- indicating the shapes to match, replicate the 'SingleProp' to the
-- same shape.
singleToParticleProp::(Elt a, Elt b)=>SingleProp a->PerParticleProp b->PerParticleProp a
singleToParticleProp (SingleProp x) (PerParticleProp template) = PerParticleProp x'
  where
    x' = M.map (broadcast x) template
    broadcast from to = A.replicate (A.lift $ Z:.(A.size to)) from

-- | Lift a constant value into a 'SingleProp'
constToSingleProp::Elt a=>a->SingleProp a
constToSingleProp = SingleProp . A.unit . A.constant
