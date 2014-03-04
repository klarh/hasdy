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

type PBundle p = Bundle p ()

type ABundle a = Bundle () a

emptyBundle = Bundle () ()

-- | Drop the rightmost property element in a bundle
peel::Bundle (props, a) accs->Bundle props accs
peel (Bundle (a, _) b) = Bundle a b

-- | Grab a per-particle property from the properties tuple and leave
-- the 'PerParticleProp' so other particle type values can be grabbed
-- as well
takePP::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp b) (Acc c)->Bundle (a, PerParticleProp b) (Acc (c, A.Vector b))
takePP typ (Bundle (a, b) c) = Bundle (a, b) (A.lift (c, unPerParticleProp b M.! typ))

-- | Grab a single property from the properties tuple
takeSP::(Elt b, Arrays c)=>Bundle (a, SingleProp b) (Acc c)->Bundle (a, SingleProp b) (Acc (c, A.Scalar b))
takeSP (Bundle (a, b) c) = Bundle (a, b) (A.lift (c, unSingleProp b))

-- | Grab an evaluated per-particle property from the properties tuple
-- and leave the 'PerParticleProp'' so other particle type values can
-- be grabbed as well
takePP'::(Elt b, Arrays c)=>ParticleType->Bundle (a, PerParticleProp' b) c->Bundle (a, PerParticleProp' b) (c, A.Vector b)
takePP' typ (Bundle (a, b) c) = Bundle (a, b) (c, unPerParticleProp' b M.! typ)

-- | Grab a single evaluated property from the properties tuple
takeSP'::(Elt b, Arrays c)=>Bundle (a, SingleProp' b) c->Bundle (a, SingleProp' b) (c, A.Scalar b)
takeSP' (Bundle (a, b) c) = Bundle (a, b) (c, unSingleProp' b)

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

data BLens p a p' a' = BLens {toAccs::(p->a),
                              toProps::(a->p),
                              toAccs'::(p'->a'),
                              toProps'::(a'->p')}

emptyBLens = BLens id id id id

compose::BLens a b a' b'->BLens b c b' c'->BLens a c a' c'
compose (BLens a b c d) (BLens a' b' c' d') = BLens (a' . a) (b . b') (c' . c) (d . d')

(<.) = compose

-- | Convert a function operating on property bundles to a function on acc bundles
bridge::BLens a b a' b'->BLens c d c' d'->(a->c)->(b->d)
bridge l1 l2 f = toAccs l2 . f . toProps l1

bridge'::BLens a b a' b'->BLens c d c' d'->(a'->c')->(b'->d')
bridge' l1 l2 f = toAccs' l2 . f . toProps' l1

-- | Convert an endomorphism on prop bundles to an endomorphism on acc bundles
bridge1::BLens a b a' b'->(a->a)->(b->b)
bridge1 l f = bridge l l f

-- | Convert a function operating on acc bundles to one operating on prop bundles
canal::BLens a b a' b'->BLens c d c' d'->(b->d)->(a->c)
canal l1 l2 f = toProps l2 . f . toAccs l1

canal'::BLens a b a' b'->BLens c d c' d'->(b'->d')->(a'->c')
canal' l1 l2 f = toProps' l2 . f . toAccs' l1

-- | Convert an endomorphism on acc bundles to an endomorphism on prop bundles
canal1::BLens a b a' b'->(b->b)->(a->a)
canal1 l f = canal l l f

usePP typ = BLens (takePP typ) (givePP typ) (takePP' typ) (givePP' typ)

newPP = BLens peel wrapPP peel wrapPP'

-- oh lord please tell me this type signature can be improved
-- newSP::(Elt b, Elt b1, Arrays c1, Arrays c2)=>
--        BLens (Bundle (a, SingleProp b) (Acc (c1, Scalar b))) c (Bundle (a1, SingleProp' b1) (c2, Scalar b1)) c'->
--        BLens (Bundle ((a, SingleProp b), SingleProp b) (Acc c1)) c (Bundle ((a1, SingleProp' b1), SingleProp' b1) c2) c'
newSP::(Elt b, Elt b1, Arrays c, Arrays c1)=>BLens (Bundle ((a, SingleProp b), SingleProp b) (Acc c)) (Bundle (a, SingleProp b) (Acc (c, Scalar b))) (Bundle ((a1, SingleProp' b1), SingleProp' b1) c1) (Bundle (a1, SingleProp' b1) (c1, Scalar b1))
newSP = BLens (takeSP . peel) giveSP (takeSP' . peel) giveSP'

-- runBLens::(Arrays a, Arrays b)=>((Acc a->Acc b)->a->b)->
--           (c->e)->BLens c d c' d'->BLens e f e' f'->
runBLens run1 f left right = canal' left right . liftAccs $ run1 (unliftAccs . bridge left right $ f)

-- | Convert a function on prop bundles to one on props
unliftProps::(Bundle a (Acc ())->Bundle c (Acc ()))->a->c
unliftProps f a = bundleProps . f $ Bundle a (A.use ())

unliftProps'::(Bundle a' ()->Bundle c' ())->a'->c'
unliftProps' f a' = bundleProps . f $ Bundle a' ()

-- | Convert a function on acc bundles (or array tuples) to one on
-- accs (or array tuples)
unliftAccs::(Bundle () b->Bundle () d)->b->d
unliftAccs f b = bundleAccs . f $ Bundle () b

-- | Convert a function on props to one on prop bundles
liftProps::(a->b)->Bundle a (Acc ())->Bundle b (Acc ())
liftProps f (Bundle a v) = Bundle (f a) v

liftProps'::(a'->b')->Bundle a' ()->Bundle b' ()
liftProps' f (Bundle a' ()) = Bundle (f a') ()

-- | Convert a function on Accs or Acc tuples to one on Acc or
-- tuple bundles.
liftAccs::(a->b)->Bundle () a->Bundle () b
liftAccs f (Bundle () a) = Bundle () (f a)
