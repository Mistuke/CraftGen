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
import Minecraft.Format.Block_Internal
import Minecraft.Format.Block()
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
createSchematic = toExternal . foldl' create mempty
  where create :: Schematic -> NBT -> Schematic
        create schem nbt | "Width"        === nbt = schem { scmWidth        = nbtShort nbt }
        create schem nbt | "Length"       === nbt = schem { scmLength       = nbtShort nbt }
        create schem nbt | "Height"       === nbt = schem { scmHeight       = nbtShort nbt }
        create schem nbt | "Materials"    === nbt = schem { scmMaterial     = mkMaterial (nbtString nbt) }
        create schem nbt | "Blocks"       === nbt = schem { scmBlocks       = (BinaryInternal (nbtBArr nbt) []) : scmBlocks schem }
        create schem nbt | "Data"         === nbt = schem { scmBlocks       = (BinaryInternal [] (nbtBArr nbt)): scmBlocks schem }
        create schem nbt | "Entities"     === nbt = schem { scmEntities     = [] }
        create schem nbt | "TileEntities" === nbt = schem { scmTileEntities = [] }
        create schem _                            = schem
        
        toExternal :: Schematic -> Schematic
        toExternal schem = let [a,b] = scmBlocks schem
                               (BinaryInternal a1 b1) = a
                               (BinaryInternal a2 b2) = b
                               res = BinaryInternal(a1++a2) (b1++b2)
                           in schem { scmBlocks = (to res) }

-- | Convert a schematic to a NBT format
fromSchematic :: Schematic -> NBT
fromSchematic schem 
   = NBT_TAG_Compound (mkName "Schematic")
       [ NBT_TAG_Short      (mkName "Width"       ) (scmWidth  schem)
       , NBT_TAG_Short      (mkName "Length"      ) (scmLength schem)
       , NBT_TAG_Short      (mkName "Height"      ) (scmHeight schem)
       , NBT_TAG_String     (mkName "Materials"   ) (toMaterial $ scmMaterial schem)
       , NBT_TAG_Byte_Array (mkName "Blocks"      ) blocks
       , NBT_TAG_Byte_Array (mkName "Data"        ) datas
       , NBT_TAG_List       (mkName "Entities"    ) 0 []
       , NBT_TAG_List       (mkName "TileEntities") 0 []
       ]
    where (BinaryInternal blocks datas) = from (scmBlocks schem)

