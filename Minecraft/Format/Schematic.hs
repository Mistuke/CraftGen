-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.Schematic
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains information about the schematic format
--
-----------------------------------------------------------------------------
module Minecraft.Format.Schematic 
 (module Minecraft.Format.Schematic.Data
 ) where

import Minecraft.Format.NBT
import Minecraft.Format.Schematic.Data
import Minecraft.Format.Schematic.Convertion
import Minecraft.Utils.Convertion

-- | Parse and deserialize a schematic file from disk.
--   . The expected format is an NBT format which is then converted
--   . into a schematic file.
loadSchematic :: FilePath -> IO Schematic
loadSchematic = fmap to . loadFile

-- | Save and serialize a schematic file back to a .schematic
--   files which is a gzipped NBT format
saveSchematic :: FilePath -> Schematic -> IO ()
saveSchematic file = saveFile file . from