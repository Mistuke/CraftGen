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
module Minecraft.Format.Schematic.Data
  (-- * Datatype definitions
   MaterialMode(..)
  ,Schematic(..)
  
   -- * utility functions
  , mkMaterial
  ) where

import Data.Int
import Data.Monoid

import Minecraft.Utils.CaseInSensitive
import Minecraft.Format.Block
import Minecraft.Format.Entity
import Minecraft.Format.TileEntity

-- | The 'material mode' for the schematic file.
--   These influence the parsing and the general layout
--   of the blocks
data MaterialMode 
  = ClassicMode -- ^ Minecraft Classic levels
  | AlphaMode   -- ^ Minecraft Alpha, Minecraft Beta and Minecraft worlds.
  | UnknownMode -- ^ An unknown version of schematics
  deriving (Eq, Show)
  
-- | Creates a material from the specified string
mkMaterial :: String -> MaterialMode
mkMaterial str | str === "classic" = ClassicMode
mkMaterial str | str === "alpha"   = AlphaMode
mkMaterial _                       = UnknownMode

instance Monoid MaterialMode where
    mempty = UnknownMode
    ClassicMode `mappend` ClassicMode = ClassicMode
    AlphaMode   `mappend` AlphaMode   = AlphaMode
    _           `mappend` _           = UnknownMode
  
-- | Data type representing a Schematic file.
data Schematic 
  = Schematic { scmWidth        :: Int16        -- ^ Size along the X axis. 
              , scmLength       :: Int16        -- ^ Size along the Z axis. 
              , scmHeight       :: Int16        -- ^ Size along the Y axis.
              , scmMaterial     :: MaterialMode -- ^ This will be "Classic" for schematics 
                                                --   . exported from Minecraft Classic levels, 
                                                --   . and "Alpha" for those from Minecraft Alpha,
                                                --   . Minecraft Beta and Minecraft worlds.
              , scmBlocks       :: Blocks       -- ^ Block IDs defining the terrain. 8 bits per block.
                                                --   . Block data additionally defining parts of the terrain.
                                                --   . Only the lower 4 bits of each byte are used.
              , scmEntities     :: Entities     -- ^ Each TAG_Compound in this list defines an entity in the
                                                --   . schematic. See the Entity Format for Alpha levels. 
              , scmTileEntities :: TileEntities -- ^ Each TAG_Compound in this list defines a tile entity in 
                                                --   . the schematic. See Tile Entity Format. 
              }
  deriving (Eq, Show)
  
instance Monoid Schematic where
    mempty        = Schematic 0 0 0 mempty [] [] []
    a `mappend` b = Schematic { scmWidth        = scmWidth        a +  scmWidth        b
                              , scmLength       = scmLength       a +  scmLength       b
                              , scmHeight       = scmHeight       a +  scmHeight       b
                              , scmMaterial     = scmMaterial a `mappend` scmMaterial  b
                              , scmBlocks       = scmBlocks       a ++ scmBlocks       b
                              , scmEntities     = scmEntities     a ++ scmEntities     b
                              , scmTileEntities = scmTileEntities a ++ scmTileEntities b
                              }