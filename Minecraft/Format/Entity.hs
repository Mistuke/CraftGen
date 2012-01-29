-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.Schematic.Data
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Datatype description of the Schematic format.
-- Read more at of the format at
-- http://www.minecraftwiki.net/wiki/Schematic_File_Format
-- These .schematic files are a gzipped NBT format
--
-----------------------------------------------------------------------------
module Minecraft.Format.Entity where

type Entities = [Entity]

-- | Every entity is an unnamed TAG_Compound contained 
--   in the Entities list of a chunk file. The sole exception 
--   is the Player entity, stored in level.dat.
data Entity 
  = Entity
  deriving (Eq, Show)