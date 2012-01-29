{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.Schematic.Convertion
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the required code to convert between the NBT and Schematic
-- datatypes.
--
-----------------------------------------------------------------------------
module Minecraft.Format.Schematic.Convertion() where

import Minecraft.PrettyPrinting
import Minecraft.Format.NBT
import Minecraft.Format.Schematic.Data
import Minecraft.Utils.Convertion
import Minecraft.Utils.CaseInSensitive

import Data.List
import Data.Monoid

-- | Convert an NBT to a schematic
instance Convertion NBT Schematic where
  to   = toSchematic
  from = fromSchematic
  
-- | Convert a NBT format to a schematic format
toSchematic :: NBT -> Schematic
toSchematic (NBT_TAG_Compound nm payload)
  | nm === "Schematic" = createSchematic payload
  | otherwise          = error $ "Unexpected name found, Expected 'Schematic' but got '" ++ ppRender nm ++ "'"  
toSchematic x = error $ "Expected TAG_Compound instead got" ++ take 20 (show x)
  
-- | Create a schematic from a list of NBT_Data
createSchematic :: [NBT] -> Schematic
createSchematic = foldl' create mempty
  where create :: Schematic -> NBT -> Schematic
        create schem nbt | "Width"        === nbt = schem { scmWidth        = nbtShort nbt }
        create schem nbt | "Length"       === nbt = schem { scmLength       = nbtShort nbt }
        create schem nbt | "Height"       === nbt = schem { scmHeight       = nbtShort nbt }
        create schem nbt | "Materials"    === nbt = schem { scmMaterial     = mkMaterial (nbtString nbt) }
        create schem nbt | "Blocks"       === nbt = schem { scmBlocks       = [] }
        create schem nbt | "Data"         === nbt = schem { scmData         = [] }
        create schem nbt | "Entities"     === nbt = schem { scmEntities     = [] }
        create schem nbt | "TileEntities" === nbt = schem { scmTileEntities = [] }
        create schem _                            = schem

-- | Convert a schematic to a NBT format
fromSchematic :: Schematic -> NBT
fromSchematic = undefined

