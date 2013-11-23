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

posFrame::Vec3' Float->A.Vector (Vec3' Float)->Builder
posFrame box positions = (build ("boxMatrix {} 0 0 0 {} 0 0 0 {}\n" :: Format) box `mappend`
                          "def A \"sphere 1 00cffe\"\n" `mappend`
                          mconcat (posLine <$> A.toList positions) `mappend` "eof\n")

posLine::Vec3' Float->Builder
posLine r = build ("A {} {} {}\n" :: Format) r
