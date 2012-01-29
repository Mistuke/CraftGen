-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.TileEntity
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
module Minecraft.Format.TileEntity where

-- | Known TileEntity ids: Furnace, Sign, MobSpawner, Chest, Music, Trap,
--   RecordPlayer, Piston, Cauldron, EnchantTable, and End portal 
data TileEntity
  = TileEntity
  deriving (Eq, Show)