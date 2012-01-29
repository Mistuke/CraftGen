-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.Block_Internal
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the block format definition. The internal classes provide
-- an extra constructor that should only be used by the serialization
-- and deserialization modules. Everything else should use 
-- Minecraft.Format.Block
--
-----------------------------------------------------------------------------
module Minecraft.Format.Block_Internal where

import Data.Int

type Blocks        = [Block]
type Spread        = Int8
type Counter       = Int8
type CactusData    = Int8
type SugarData     = Int8
type Wetness       = Int8
type Growth        = Int8
type TallGrassData = Int8

-- | The minecraft block format
data Block = Air                     | DeadBush             | WoodenDoor                 | Trapdoor
           | Stone                   | Piston PistonData    | Ladders                    | Silverfish
           | Grass                   | PistonExt PistonData | Rails                      | StoneBricks
           | Dirt                    | Wool Color           | CobblestoneStairs          | HugeBrownMushroom
           | Cobblestone             | BlockMovedByPiston   | WallSign                   | HugeRedMushroom
           | WoodenPlanks            | Dandelion            | Lever                      | IronBars
           | Saplings SaplingData    | Rose                 | StonePressurePlate         | GlassPane
           | Bedrock                 | BrownMushroom        | IronDoor                   | Melon
           | Water LiquidData        | RedMushroom          | WoodenPressurePlate        | PumpkinStem
           | StillWater LiquidData   | Gold                 | RedstoneOre                | MelonStem
           | Lava LiquidData         | Iron                 | GlowingRedstoneOre         | Vines
           | StillLava LiquidData    | DoubleSlabs SlabData | RedstoneTorchOff TorchData | FenceGate
           | Sand                    | Slabs SlabData       | RedstoneTorchOn  TorchData | BrickStairs
           | Gravel                  | Bricks               | StoneButton                | StoneBrickStairs
           | GoldOre                 | TNT                  | Snow                       | Mycelium
           | IronOre                 | Bookshelf            | Ice                        | LilyPad
           | CoalOre CoalData        | Moss                 | SnowBlock                  | NetherBrick
           | Wood WoodData           | Obsidian             | Cactus CactusData          | NetherBrickFence
           | Leaves LeavesData       | Torch TorchData      | ClayBlock                  | NetherBrickStairs
           | Sponge                  | Fire Spread          | SugarCane SugarData        | NetherWart
           | Glass                   | MonsterSpawner       | Jukebox JukeboxData        | EnchantmentTable
           | LapisLazuliOre          | WoodenStairs         | Fence                      | BrewingStand
           | LapisLazuliBlock        | Chest                | Pumpkin                    | Cauldron
           | Dispenser Direction     | RedstoneWire         | Netherrack                 | EndPortal
           | Sandstone               | DiamondOre           | SoulSand                   | EndPortalFrame
           | NoteBlock               | Diamond              | Glowstone                  | EndStone
           | Bed BedData             | CraftingTable        | Portal                     | DragonEgg
           | PoweredRail  RailData   | WheatSeeds           | JackOLantern        
           | DetectorRail RailData   | Farmland Wetness     | CakeBlock            
           | StickyPiston PistonData | Furnace              | RedstoneRepeaterOff  
           | Cobweb                  | BurningFurnace       | RedstoneRepeaterOn   
           | TallGrass TallGrassData | SignPost             | LockedChest         
           | UnknownBlock Int8            -- ^ The block Id is unknown.
           | BinaryInternal [Int8] [Int8] -- ^ For internal use only!
  deriving (Show, Eq)
  
-- | Wood data values
data WoodData = OakWood
              | PineWood
              | BirchWood
              | JungleWood
  deriving (Show, Eq)
              
-- | Leaves data values
data LeavesData = OakLeaves    Decay
                | PineLeaves   Decay
                | BirchLeaves  Decay
                | JungleLeaves Decay
  deriving (Show, Eq)
  
-- | Indicates if the block decays over time               
data Decay = Perminant
           | Decays
  deriving (Show, Eq)
  
-- | Coal data values
data CoalData = Coal
              | Charcoal
  deriving (Show, Eq)
  
-- | Jukebox data values
data JukeboxData = NoDisc
                 | GoldDisc      -- ^ 13
                 | GreenDisc     -- ^ cat
                 | OrangeDisc    -- ^ blocks
                 | RedDisc       -- ^ chirp
                 | LimeGreenDisc -- ^ far
                 | PurpleDisc    -- ^ mall
                 | VioletDisc    -- ^ mellohi
                 | BlackDisc     -- ^ stal
                 | WhiteDisc     -- ^ strad
                 | SeaGreenDisc  -- ^ ward
                 | BrokenDisc    -- ^ 11
  deriving (Show, Eq)
  
-- | Sapling data values
data SaplingData = SaplingData Counter SaplingType
  deriving (Show, Eq)
  
-- | Sapling value type
data SaplingType = OakSapling
                 | SpruceSapling
                 | BirchSapling
                 | DroppedJungleLeaves
                 | FixedJungleLeaves
  deriving (Show, Eq)
  
-- | Liquid data values
data LiquidData = LiquidFull
                | LiquidFalling
                | LiquidStep Int8
  deriving (Show, Eq)
  
-- | Direction of a block
data Direction = FacingNorth
               | FacingSouth
               | FacingWest
               | FacingEast
  deriving (Show, Eq)
  
-- | Beds data values
--   . 0x0: Head is pointing south 
--   . 0x1: Head is pointing west 
--   . 0x2: Head is pointing north 
--   . 0x3: Head is pointing east 
--   . 0x4: (bit flag) - When 0, the bed is empty. When 1, the bed is occupied. 
--   . 0x8: (bit flag) - When 0, the foot of the bed. When 1, the head of the bed.
data BedData = BedData Direction Bool Bool
  deriving (Show, Eq)
  
-- | Rails data values
--   . Regular Minecart rails use values above 0x5 for the corner pieces. 
--   . For powered rails, 0x8 is a bit flag indicating whether or not it is 
--   . powered, and the bottom three bits have a valid range of 0x0 to 0x5. 
--   . 
--   . 0x0: flat track going north-south 
--   . 0x1: flat track going west-east 
--   . 0x2: track ascending to the east 
--   . 0x3: track ascending to the west 
--   . 0x4: track ascending to the north 
--   . 0x5: track ascending to the south 
--   . Regular minecart tracks can make a circle from four rails: 
--   . 
--   . 0x6: northwest corner (connecting east and south) 
--   . 0x7: northeast corner (connecting west and south) 
--   . 0x8: southeast corner (connecting west and north) 
--   . 0x9: southwest corner (connecting east and north) 
data RailData = FlatGoingNorthSouth
              | FlatGoingWestEast
              | TrackAscendingEast
              | TrackAscendingWest
              | TrackAscendingNorth
              | TrackAscendingSouth
              | CornerConnectEastSouth
              | CornerConnectWestSouth
              | CornerConnectWestNorth
              | CornerConnectEastNorth
  deriving (Show, Eq)
  
-- | Direction Axis
data Axis = Down | Up | North | South | West | East
  deriving (Show, Eq)
   
-- | Piston data values
--   . The top bit (0x8) is a status bit that determines whether the piston
--   . is pushed out or not. 1 for pushed out, 0 for retracted. 
--   . 
--   . The bottom three bits are a value from 0 to 5, indicating the direction 
--   . of the piston (the direction the piston head is pointing) 
data PistonData = PistonData Bool Axis
  deriving (Show, Eq)
  
-- | Color enumeration, used mostly with Wool
data Color = White     | LightGray
           | Orange    | Cyan
           | Magenta   | Purple
           | LightBlue | Blue
           | Yellow    | Brown
           | Lime      | Green
           | Pink      | Red
           | Gray      | Black
  deriving (Show, Eq)
  
-- | Slab and Double Slab material data values
data SlabData = StoneSlab
              | SandstoneSlab
              | WoodenSlab
              | CobblestoneSlab
              | BrickSlab
              | StoneBrickSlab
              | StoneSlab2
  deriving (Show, Eq)

-- | Torches and Redstone Torches data values
data TorchData = PointEast
               | PointWest
               | PointSouth
               | PointNorth
               | Standing
  deriving (Show, Eq)