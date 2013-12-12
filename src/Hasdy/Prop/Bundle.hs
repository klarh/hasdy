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

module Hasdy.Prop.Bundle where

import Data.Array.Accelerate as A
import qualified Data.Map as M

import Hasdy.Prop
import Hasdy.Types

-- | Bundle type for tupling and untupling 'PerParticleProp's and 'SingleProps'
data Bundle props accs = Bundle {bundleProps::props, bundleAccs::accs}

emptyBundle = Bundle () ()

-- | Drop the rightmost 'PerParticle' element in the properties tuple
peelPP::Elt a=>Bundle (props, PerParticleProp a) accs->Bundle props accs
peelPP (Bundle (a, _) b) = Bundle a b

-- | Drop the rightmost 'PerParticle'' element in an evaluated properties tuple
peelPP'::Elt a=>Bundle (props, PerParticleProp' a) accs->Bundle props accs
peelPP' (Bundle (a, _) b) = Bundle a b

-- | Grab a per-particle property from the properties tuple and leave
-- the 'PerParticleProp' so other particle type values can be grabbed
-- as well
takePP::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp b) (Acc c)->Bundle (a, PerParticleProp b) (Acc (c, A.Vector b))
takePP typ (Bundle (a, b) c) = Bundle (a, b) (A.lift (c, unPerParticleProp b M.! typ))

-- | Grab (and remove) a single property from the properties tuple
takeSP::(Elt b, Arrays c)=>Bundle (a, SingleProp b) (Acc c)->Bundle a (Acc (c, A.Scalar b))
takeSP (Bundle (a, b) c) = Bundle a (A.lift (c, unSingleProp b))

-- | Grab an evaluated per-particle property from the properties tuple
-- and leave the 'PerParticleProp'' so other particle type values can
-- be grabbed as well
takePP'::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp' b) c->Bundle (a, PerParticleProp' b) (c, A.Vector b)
takePP' typ (Bundle (a, b) c) = Bundle (a, b) (c, unPerParticleProp' b M.! typ)

-- | Grab (and remove) a single evaluated property from the properties tuple
takeSP'::(Elt b, Arrays c)=>Bundle (a, SingleProp' b) c->Bundle a (c, A.Scalar b)
takeSP' (Bundle (a, b) c) = Bundle a (c, unSingleProp' b)

-- | Add an empty 'PerParticleProp' to the properties tuple
wrapPP::Bundle props accs->Bundle (props, PerParticleProp a) accs
wrapPP (Bundle props accs) = Bundle (props, PerParticleProp M.empty) accs

-- | Add an empty 'PerParticleProp'' to the properties tuple
wrapPP'::Bundle props accs->Bundle (props, PerParticleProp' a) accs
wrapPP' (Bundle props accs) = Bundle (props, PerParticleProp' M.empty) accs

-- | Pull a vector off of an Acc bundle and put it into a 'PerParticleProp'
givePP::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp b) (Acc (c, A.Vector b))->Bundle (a, PerParticleProp b) (Acc c)
givePP typ (Bundle (a, b) v) = Bundle (a, b') (A.fst v)
  where
    b' = PerParticleProp . M.insert typ (A.snd v) . unPerParticleProp $ b

-- | Pull a scalar value off of an Acc bundle and put it into a 'SingleProp'
giveSP::(Elt b, Arrays c)=>Bundle a (Acc (c, A.Scalar b))->Bundle (a, SingleProp b) (Acc c)
giveSP (Bundle a v) = Bundle (a, SingleProp (A.snd v)) (A.fst v)

-- | Pull a vector off of an array bundle and put it into a 'PerParticleProp''
givePP'::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp' b) (c, A.Vector b)->Bundle (a, PerParticleProp' b) c
givePP' typ (Bundle (a, b) v) = Bundle (a, b') (Prelude.fst v)
  where
    b' = PerParticleProp' . M.insert typ (Prelude.snd v) . unPerParticleProp' $ b

-- | Pull a scalar value off of an array bundle and put it into a 'SingleProp''
giveSP'::(Elt b, Arrays c)=>Bundle a (c, A.Scalar b)->Bundle (a, SingleProp' b) c
giveSP' (Bundle a v) = Bundle (a, SingleProp' (Prelude.snd v)) (Prelude.fst v)
