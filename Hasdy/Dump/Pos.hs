{-# LANGUAGE OverloadedStrings #-}
module Hasdy.Dump.Pos where

import Prelude as P
import Data.Array.Accelerate as A
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Text.Format (Format(..), build)
import Data.Monoid (mconcat)
import Control.Applicative ((<$>))

import Hasdy.Vectors(Vec3'(..))

posFrame::A.Vector (Vec3' Float)->Builder
posFrame positions = mconcat (posLine <$> A.toList positions)

posLine::Vec3' Float->Builder
posLine r = build ("{} {} {}\n" :: Format) r
