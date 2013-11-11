-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Hasdy.Vectors where

import Data.Array.Accelerate (Acc, Exp)
import Data.Array.Accelerate as A
import Data.NumInstances

type Vec2' a = (a, a)
type Vec3' a = (a, a, a)
type Vec4' a = (a, a, a, a)

type Vec2 a = Exp (Vec2' a)
type Vec3 a = Exp (Vec3' a)
type Vec4 a = Exp (Vec4' a)

zipWith3'::(a->b->c)->Vec3' a->Vec3' b->Vec3' c
zipWith3' f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)

zip3'::Vec3' a->Vec3' b->Vec3' (a, b)
zip3' = Hasdy.Vectors.zipWith3' (\x y->(x, y))

map3'::(a->b)->Vec3' a->Vec3' b
map3' f (x, y, z) = (f x, f y, f z)

fold3'::(a->a->a)->Vec3' a->a
fold3' f (x, y, z) = x `f` y `f` z

pure3'::a->Vec3' a
pure3' x = (x, x, x)

zipWith4'::(a->b->c)->Vec4' a->Vec4' b->Vec4' c
zipWith4' f (w1, x1, y1, z1) (w2, x2, y2, z2) =
  (f w1 w2, f x1 x2, f y1 y2, f z1 z2)

zip4'::Vec4' a->Vec4' b->Vec4' (a, b)
zip4' = Hasdy.Vectors.zipWith4' (\x y->(x, y))

map4'::(a->b)->Vec4' a->Vec4' b
map4' f (w, x, y, z) = (f w, f x, f y, f z)

fold4'::(a->a->a)->Vec4' a->a
fold4' f (w, x, y, z) = w `f` x `f` y `f` z

pure4'::a->Vec4' a
pure4' x = (x, x, x, x)

zipWith3::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
            Vec3 a->Vec3 b->Vec3 c
zipWith3 f v1 v2 = A.lift (f x1 x2, f y1 y2, f z1 z2)
  where
    (x1, y1, z1) = A.unlift v1
    (x2, y2, z2) = A.unlift v2

zip3::(Elt a, Elt b)=>Vec3 a->Vec3 b->Vec3 (a, b)
zip3 = Hasdy.Vectors.zipWith3 (\x y->A.lift (x, y))

map3::(Elt a, Elt b)=>(Exp a->Exp b)->Vec3 a->Vec3 b
map3 f v = A.lift (f x, f y, f z)
  where
    (x, y, z) = A.unlift v

fold3::Elt a=>(Exp a->Exp a->Exp a)->Vec3 a->Exp a
fold3 f v = x `f` y `f` z
  where
    (x, y, z) = A.unlift v

pure3::Elt a=>Exp a->Vec3 a
pure3 x = A.lift (x, x, x)

zipWith4::(Elt a, Elt b, Elt c)=>(Exp a->Exp b->Exp c)->
            Vec4 a->Vec4 b->Vec4 c
zipWith4 f v1 v2 = A.lift (f w1 w2, f x1 x2, f y1 y2, f z1 z2)
  where
    (w1, x1, y1, z1) = A.unlift v1
    (w2, x2, y2, z2) = A.unlift v2

zip4::(Elt a, Elt b)=>Vec4 a->Vec4 b->Vec4 (a, b)
zip4 = Hasdy.Vectors.zipWith4 (\x y->A.lift (x, y))

map4::(Elt a, Elt b)=>(Exp a->Exp b)->Vec4 a->Vec4 b
map4 f v = A.lift (f w, f x, f y, f z)
  where
    (w, x, y, z) = A.unlift v

fold4::Elt a=>(Exp a->Exp a->Exp a)->Vec4 a->Exp a
fold4 f v = w `f` x `f` y `f` z
  where
    (w, x, y, z) = A.unlift v

pure4::Elt a=>Exp a->Vec4 a
pure4 x = A.lift (x, x, x, x)

-- Elementwise Vec3' operations
plus3::(Elt a, IsNum a)=>Vec3 a->Vec3 a->Vec3 a
plus3 = Hasdy.Vectors.zipWith3 (+)
minus3::(Elt a, IsNum a)=>Vec3 a->Vec3 a->Vec3 a
minus3 = Hasdy.Vectors.zipWith3 (-)
times3::(Elt a, IsNum a)=>Vec3 a->Vec3 a->Vec3 a
times3 = Hasdy.Vectors.zipWith3 (*)
div3::(Elt a, IsFloating a)=>Vec3 a->Vec3 a->Vec3 a
div3 = Hasdy.Vectors.zipWith3 (/)

scale3 s v = pure3 s `times3` v

-- normalize3::(Elt a, IsFloating (a, a, a))=>Vec3 a->Vec3 a
-- normalize3 v = v / pure3 (length3 v)

-- Common Vec3' reductions
dot3::(Elt a, IsNum a)=>Vec3 a->Vec3 a->Exp a
dot3 v1 v2 = fold3 (+) $ v1 `times3` v2
length3::(Elt a, IsFloating a)=>Vec3 a->Exp a
length3 v = sqrt $ dot3 v v
-- mkDim3::(Vec3 Int)->Exp DIM3
-- mkDim3 v = A.lift $ (Z:. x:. y:. z)
--   where
--     (x, y, z) = A.unlift v :: (Exp Int, Exp Int, Exp Int)
-- unDim3::Exp DIM3->(Vec3 Int)
-- unDim3 v = A.lift $ (x, y, z)
--   where
--     (Z:. x:. y:. z) = A.unlift v :: (Z:. Exp Int:. Exp Int:. Exp Int)

-- Elementwise Vec4' operations
plus4::(Elt a, IsNum a)=>Vec4 a->Vec4 a->Vec4 a
plus4 = Hasdy.Vectors.zipWith4 (+)
minus4::(Elt a, IsNum a)=>Vec4 a->Vec4 a->Vec4 a
minus4 = Hasdy.Vectors.zipWith4 (-)
times4::(Elt a, IsNum a)=>Vec4 a->Vec4 a->Vec4 a
times4 = Hasdy.Vectors.zipWith4 (*)
div4::(Elt a, IsFloating a)=>Vec4 a->Vec4 a->Vec4 a
div4 = Hasdy.Vectors.zipWith4 (/)

-- normalize4::(Elt a, IsFloating a)=>Vec4 a->Vec4 a
-- normalize4 v = v `div4` pure4 (length4 v)

-- Common Vec4' reductions
dot4::(Elt a, IsNum a)=>Vec4 a->Vec4 a->Exp a
dot4 v1 v2 = fold4 (+) $ times4 v1 v2
length4::(Elt a, IsFloating a)=>Vec4 a->Exp a
length4 v = sqrt $ dot4 v v
