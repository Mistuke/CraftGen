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
-- .
-- See more at http://www.minecraftwiki.net/wiki/Data_values
-----------------------------------------------------------------------------
module Minecraft.Format.Block_Internal where

import Data.Int

type Blocks  = [Block]
type Opened  = Bool
type Pressed = Bool
type Powered = Bool
type Counter = Int8
type Checked = Bool

newtype Spread        = Spread        { unSpread     :: Int8 } deriving (Show, Eq)
newtype CactusData    = CactusData    { unCactusData :: Int8 } deriving (Show, Eq)
newtype SugarData     = SugarData     { unSugarData  :: Int8 } deriving (Show, Eq)
newtype Wetness       = Wetness       { unWetness    :: Int8 } deriving (Show, Eq)
newtype Growth        = Growth        { unGrowth     :: Int8 } deriving (Show, Eq)
newtype TallGrassData = TallGrassData { unTGData     :: Int8 } deriving (Show, Eq)
newtype Level         = Level         { unLevel      :: Int8 } deriving (Show, Eq)
newtype Delay         = Delay         { unDelay      :: Int8 } deriving (Show, Eq)

newtype PumpKData     = PumpKData     { unPumpK      :: Direction } deriving (Show, Eq)
newtype StairsData    = StairsData    { unStairs     :: Direction } deriving (Show, Eq)

-- | The minecraft block format
data Block = Air                     | DeadBush                | WoodenDoor DoorData              | Trapdoor TrapDoorData          | EmeraldOre
           | Stone                   | Piston PistonData       | Ladders Direction                | Silverfish SilverFishData      | EnderChest
           | Grass                   | PistonExt PistonData    | Rails RailData                   | StoneBricks StoneData          | TripwireHook
           | Dirt                    | Wool Color              | CobblestoneStairs StairsData     | HugeBrownMushroom MushroomData | Tripwire
           | Cobblestone             | BlockMovedByPiston      | WallSign Direction               | HugeRedMushroom MushroomData   | BlockOfEmerald
           | WoodenPlanks            | Dandelion               | Lever LeverData                  | IronBars                       | SpruceWoodStairs
           | Saplings SaplingData    | Rose                    | StonePressurePlate Pressed       | GlassPane                      | BirchWoodStairs
           | Bedrock                 | BrownMushroom           | IronDoor DoorData                | Melon                          | JungleWoodStairs
           | Water LiquidData        | RedMushroom             | WoodenPressurePlate Pressed      | PumpkinStem Growth
           | StillWater LiquidData   | Gold                    | RedstoneOre                      | MelonStem Growth
           | Lava LiquidData         | Iron                    | GlowingRedstoneOre               | Vines Direction
           | StillLava LiquidData    | DoubleSlabs SlabData    | RedstoneTorchOff TorchData       | FenceGate TrapDoorData
           | Sand                    | Slabs SlabData          | RedstoneTorchOn  TorchData       | BrickStairs StairsData
           | Gravel                  | Bricks                  | StoneButton ButtonData           | StoneBrickStairs StairsData
           | GoldOre                 | TNT                     | Snow Level                       | Mycelium
           | IronOre                 | Bookshelf               | Ice                              | LilyPad
           | CoalOre                 | Moss                    | SnowBlock                        | NetherBrick
           | Wood WoodData           | Obsidian                | Cactus CactusData                | NetherBrickFence
           | Leaves LeavesData       | Torch TorchData         | ClayBlock                        | NetherBrickStairs StairsData
           | Sponge                  | Fire Spread             | SugarCane SugarData              | NetherWart Growth
           | Glass                   | MonsterSpawner          | Jukebox JukeboxData              | EnchantmentTable
           | LapisLazuliOre          | WoodenStairs StairsData | Fence                            | BrewingStand BrewingData
           | LapisLazuliBlock        | Chest Direction         | Pumpkin PumpKData                | Cauldron CauldronData
           | Dispenser Direction     | RedstoneWire Spread     | Netherrack                       | EndPortal
           | Sandstone               | DiamondOre              | SoulSand                         | EndPortalFrame PortalFrameData
           | NoteBlock               | Diamond                 | Glowstone                        | EndStone
           | Bed BedData             | CraftingTable           | Portal                           | DragonEgg
           | PoweredRail  RailData   | WheatSeeds Growth       | JackOLantern PumpKData           | RedstoneLambInactive
           | DetectorRail RailData   | Farmland Wetness        | CakeBlock                        | RedstoneLambActive
           | StickyPiston PistonData | Furnace Direction       | RedstoneRepeaterOff RepeaterData | WoodenDoubleSlab
           | Cobweb                  | BurnFurnace Direction   | RedstoneRepeaterOn  RepeaterData | CocaoPlant
           | TallGrass TallGrassData | SignPost SignData       | LockedChest                      | SandstoneStairs
           | UnknownBlock Int8            -- ^ The block Id is unknown.
           | BinaryInternal [Int8] [Int8] -- ^ For internal use only!
           | UnusedPayload [Int8]         -- ^ For some reason there were too many payloads given
  deriving (Show, Eq)
  
-- | Wood data values
data WoodData = OakWood
              | PineWood
              | BirchWood
              | JungleWood
  deriving (Show, Eq)
              
-- | Leaves data values
data LeavesData = OakLeaves    { unLd :: Decay, unLc :: Checked }
                | PineLeaves   { unLd :: Decay, unLc :: Checked }
                | BirchLeaves  { unLd :: Decay, unLc :: Checked }
                | JungleLeaves { unLd :: Decay, unLc :: Checked }
  deriving (Show, Eq)
  
-- | Indicates if the block decays over time               
data Decay = Permanent
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
data LiquidData = LiquidFull Bool
                | LiquidStep Int8 Bool
  deriving (Show, Eq)
  
-- | Direction of a block
data Direction = ToNorth
               | ToSouth
               | ToWest
               | ToEast
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
data RailData = RailData Powered RailData
              | FlatGoingNorthSouth
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
  
-- | Sign post data values
data SignData = PostSouth           | PostNorth 
              | PostSouth_SouthWest | PostNorth_NorthEast
              | PostSouthWest       | PostNorthEast 
              | PostWest_SouthWest  | PostEast_NorthEast 
              | PostWest            | PostEast 
              | PostWest_NorthWest  | PostEast_SouthEast 
              | PostNorthWest       | PostSouthEast 
              | PostNorth_NorthWest | PostSouth_SouthEast
  deriving (Show, Eq)
  
-- | Door data values
--   . The two least significant bits are the orientation of the door,
--   . that is, the corner in which its hinge is positioned: 
--   . 
--   . 0x0: northwest corner 
--   . 0x1: northeast corner 
--   . 0x2: southeast corner 
--   . 0x3: southwest corner 
--   . The two bits above are flags: 
--   . 
--   . 0x8: If this bit is set, this is the top half of a door (else the lower half). 
--   . 0x4: If this bit is set, the door has swung counterclockwise around its hinge. 
--   . For example, the bottom half of a door with its hinge on the southwest corner, 
--   . which is swung so that it is closed when viewed from the west, will have a data
--   .  of (3 | 4) = (3 + 4) = 7. 
data DoorData = DoorData CornerDirection Bool Bool
  deriving (Show, Eq)
  
-- | Corner direction data values, see \DoorData\
data CornerDirection = CornerNorthWest
                     | CornerNorthEast
                     | CornerSouthEast
                     | CornerSouthWest
  deriving (Show, Eq)
  
-- | Levers control data values
--   . 0x8: If this bit is set, the lever has been thrown and is providing power. 
--   . Wall levers: 
--   . 
--   . 0x1: Facing east 
--   . 0x2: Facing west 
--   . 0x3: Facing south 
--   . 0x4: Facing north 
--   . Ground levers: 
--   . 
--   . 0x5: Lever points south when off. 
--   . 0x6: Lever points east when off. (Note that unlike the other types of switch, 
--   .      this version didn't power wires around the block it was sitting on. This
--   .      bug was fixed in Beta 1.6) 
data LeverData = WallLever Powered Direction
               | GroundLever Powered Direction
  deriving (Show, Eq)
  
-- | Button data values
data ButtonData = ButtonData Pressed Direction
  deriving (Show, Eq)
  
-- | Repeater data values
data RepeaterData = RepeaterData Direction Delay
  deriving (Show, Eq)
  
-- | Door data values
data TrapDoorData = TrapDoorData Opened Direction
  deriving (Show, Eq)
  
-- | Silver fish data values
data SilverFishData = SfStone
                    | SfCobbleStone
                    | SfStoneBrick
  deriving (Show, Eq)
  
-- | Stone brick data values
data StoneData = NormalStone | MossyStone | CrackedStone
  deriving (Show, Eq)
  
-- | Mushroom data values
--   . Value  Description   Textures  
--   . 0      Fleshy piece  Pores on all sides  
--   . 1      Corner piece  Cap texture on top, west and north  
--   . 2      Side piece    Cap texture on top and north  
--   . 3      Corner piece  Cap texture on top, north and east  
--   . 4      Side piece    Cap texture on top and west  
--   . 5      Top piece     Cap texture on top  
--   . 6      Side piece    Cap texture on top and east  
--   . 7      Corner piece  Cap texture on top, south and west  
--   . 8      Side piece    Cap texture on top and south  
--   . 9      Corner piece  Cap texture on top, east and south  
--   . 10     Stem piece    Stem texture on all four sides, pores on top and bottom 
data MushroomData = MushroomFleshy
                  | MushroomCapTopWestNorth
                  | MushroomCapTopNorth
                  | MushroomCapTopNorthEast
                  | MushroomCapTopWest
                  | MushroomCapTop
                  | MushroomCapTopEast
                  | MushroomCapTopSouthWest
                  | MushroomCapTopSouth
                  | MushroomCapTopEastSouth
                  | MushroomStem
  deriving (Show, Eq)
  
-- | Brewing data values
--   . The bottom three bits are bit flags for which bottle slots actually contain bottles. 
--   . The actual bottle contents (and the reagent at the top) are stored in a TileEntity for 
--   . this block, not in the data field. 
--   . 
--   . 0x1: The slot pointing east 
--   . 0x2: The slot pointing southwest 
--   . 0x4: The slot pointing northwest 
data BrewingData = BrewingData Bool Bool Bool
  deriving (Show, Eq)
  
-- | Cauldron data values
data CauldronData = CauldronEmpty
                  | CauldronThird
                  | CauldronHalf
                  | CauldronFull
  deriving (Show, Eq)
  
-- | Portal frame data values
--   . The bottom two bits determine which "side" of 
--   . the whole portal frame this block is a part of. 
--   . 
--   . 0x4 is a bit flag: 0 is an "empty" frame block, 1 
--   . is a block with an Eye of Ender inserted. 
data PortalFrameData = PortalFrameData Direction Bool
  deriving (Show, Eq)