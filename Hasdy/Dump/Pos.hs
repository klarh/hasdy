{-# LANGUAGE OverloadedStrings #-}
module Hasdy.Dump.Pos where

import Prelude as P
import Data.Array.Accelerate as A
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder(..))
import Data.Text.Format (Format(..), build)
import Data.Monoid (mappend, mconcat)
import Control.Applicative ((<$>))

import Hasdy.Vectors(Vec3'(..))

posFrame::A.Vector (Vec3' Float)->Builder
posFrame positions = ("def A \"Sphere 1\"\n" `mappend`
                      mconcat (posLine <$> A.toList positions) `mappend`
                     "EOF\n")

posLine::Vec3' Float->Builder
posLine r = build ("A {} {} {}\n" :: Format) r
