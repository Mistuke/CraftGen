{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.Block
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the block format definition. This class should be used in 
-- everything but the code generation modules and is the one to be exported.
-- Based on http://www.minecraftwiki.net/wiki/Data_values
--
-----------------------------------------------------------------------------
module Minecraft.Format.Block 
  (module Minecraft.Format.Block_Internal
  )where

import Data.Bits
import Data.Int
import Data.List

import Minecraft.Utils.Convertion
  
import Minecraft.Format.Block_Internal hiding (Block (BinaryInternal))
import qualified Minecraft.Format.Block_Internal as BL 

-- | Todo define this convertion
instance Convertion BL.Block Blocks where
  to (BL.BinaryInternal blocks datas) = mkBlocks blocks datas
  to _                                = error $ "Expected an Internal node, got a real block instead"
  
  from x                    = BL.BinaryInternal [] []

-- | Create blocks from a list of bytes and payload  
mkBlocks :: [Int8] -> [Int8] -> Blocks
mkBlocks []     []      = []
mkBlocks (x:xs) payload 
  = let (x', pay') = case x of
                       0   -> lit Air 
                       1   -> lit Stone
                       2   -> lit Grass
                       3   -> lit Dirt
                       4   -> lit Cobblestone
                       5   -> lit WoodenPlanks
                       6   -> lat Saplings
                       7   -> lit Bedrock
                       8   -> lat Water
                       9   -> lat StillWater
                       10  -> lat Lava
                       11  -> lat StillLava
                       12  -> lit Sand
                       13  -> lit Gravel
                       14  -> lit GoldOre
                       15  -> lit IronOre
                       16  -> lit CoalOre
                       17  -> lat Wood
                       18  -> lat Leaves
                       19  -> lit Sponge
                       20  -> lit Grass
                       21  -> lit LapisLazuliOre
                       22  -> lit LapisLazuliBlock
                       23  -> lat Dispenser
                       24  -> lit Sandstone
                       25  -> lit NoteBlock
                       26  -> lat Bed
                       27  -> lat PoweredRail
                       28  -> lat DetectorRail
                       29  -> lat StickyPiston
                       30  -> lit Cobweb
                       31  -> lat TallGrass
                       32  -> lit DeadBush              
                       33  -> lat Piston      
                       34  -> lat PistonExt  
                       35  -> lat Wool             
                       36  -> lit BlockMovedByPiston    
                       37  -> lit Dandelion             
                       38  -> lit Rose                  
                       39  -> lit BrownMushroom         
                       40  -> lit RedMushroom           
                       41  -> lit Gold                  
                       42  -> lit Iron                  
                       43  -> lat DoubleSlabs   
                       44  -> lat Slabs        
                       45  -> lit Bricks                
                       46  -> lit TNT                   
                       47  -> lit Bookshelf             
                       48  -> lit Moss                  
                       49  -> lit Obsidian              
                       50  -> lat Torch        
                       51  -> lat Fire           
                       52  -> lit MonsterSpawner        
                       53  -> lat WoodenStairs
                       54  -> lat Chest       
                       55  -> lat RedstoneWire   
                       56  -> lit DiamondOre            
                       57  -> lit Diamond               
                       58  -> lit CraftingTable         
                       59  -> lat WheatSeeds      
                       60  -> lat Farmland      
                       61  -> lat Furnace     
                       62  -> lat BurnFurnace 
                       63  -> lat SignPost     
                       64  -> lat WoodenDoor          
                       65  -> lat Ladders           
                       66  -> lat Rails              
                       67  -> lat CobblestoneStairs 
                       68  -> lat WallSign          
                       69  -> lat Lever             
                       70  -> lat StonePressurePlate  
                       71  -> lat IronDoor           
                       72  -> lat WoodenPressurePlate 
                       73  -> lit RedstoneOre                 
                       74  -> lit GlowingRedstoneOre          
                       75  -> lat RedstoneTorchOff  
                       76  -> lat RedstoneTorchOn  
                       77  -> lat StoneButton      
                       78  -> lat Snow                  
                       79  -> lit Ice                         
                       80  -> lit SnowBlock                   
                       81  -> lat Cactus           
                       82  -> lit ClayBlock                   
                       83  -> lat SugarCane         
                       84  -> lat Jukebox         
                       85  -> lit Fence                       
                       86  -> lat Pumpkin           
                       87  -> lit Netherrack                  
                       88  -> lit SoulSand                    
                       89  -> lit Glowstone                   
                       90  -> lit Portal                      
                       91  -> lat JackOLantern      
                       92  -> lit CakeBlock            
                       93  -> lat RedstoneRepeaterOff
                       94  -> lat RedstoneRepeaterOn
                       95  -> lit LockedChest         
                       96  -> lat Trapdoor 
                       97  -> lat Silverfish 
                       98  -> lat StoneBricks 
                       99  -> lat HugeBrownMushroom 
                       100 -> lat HugeRedMushroom
                       101 -> lit IronBars
                       102 -> lit GlassPane
                       103 -> lit Melon
                       104 -> lat PumpkinStem
                       105 -> lat MelonStem
                       106 -> lat Vines
                       107 -> lat FenceGate
                       108 -> lat BrickStairs
                       109 -> lat StoneBrickStairs
                       110 -> lit Mycelium
                       111 -> lit LilyPad
                       112 -> lit NetherBrick
                       113 -> lit NetherBrickFence
                       114 -> lat NetherBrickStairs
                       115 -> lat NetherWart
                       116 -> lit EnchantmentTable
                       117 -> lat BrewingStand
                       118 -> lat Cauldron
                       119 -> lit EndPortal
                       120 -> lat EndPortalFrame
                       121 -> lit EndStone
                       122 -> lit DragonEgg

                       _   -> error $ "Unrecognized block with Id " ++ show x
                       
        lit y = (y, payload)
        
        lat :: Convertion Int8 a => (a -> BL.Block) -> (BL.Block, [Int8])
        lat y = if null payload
                   then error "Expected payload but found none"
                   else let (p':px) = payload
                        in (y (to p'), px)
    in x' : mkBlocks xs pay'
    
-- | Get the value of the ith bit
getBit :: Bits a => a -> Int -> a
getBit x i = if x `testBit` i then 1 else 0

-- | Sum the value of the given bits
--   taking the first bit to always be the
--   lowest bit value 0.
sumBits :: Bits a => a -> [Int] -> a
sumBits x bits = sum' (sort bits ) 0
  where sum' [] _     = 0
        sum' (b:bs) y = (x `getBit` b * 2^y) + sum' bs (y+1)
                 
    
-- | Convert a Byte to a SaplingData
--   . As of Beta 1.5, the data value is split in half. 
--   . The bottom two bits now determine the type of sapling 
--   . (and thus the eventual tree type), according to the following table:
--   . Value  Description
--   . 	0 	  Oak Sapling
--   . 	1 	  Spruce Sapling
--   . 	2 	  Birch Sapling
--   . 	3 	  Dropped by Jungle Leaves, but otherwise is oak sapling
--   . 	4+ 	  Same as 3, but never dropped
--   . 
--   . The top two bits function as the counter, as in pre-1.5 builds, though obviously there are only four values available in the remaining bits. Either the counter is incremented more slowly to compensate, or trees might just grow more quickly since the update. 
instance Convertion Int8 SaplingData where
    to x = let kind  = case sumBits x [0,1] of
                         0 -> OakSapling
                         1 -> SpruceSapling
                         2 -> BirchSapling
                         3 -> DroppedJungleLeaves
                         _ -> FixedJungleLeaves
               count = sumBits x [2,3]
           in SaplingData count kind
    from (SaplingData count kind) 
         = let base  = 0 :: Int8
               base' = case kind of
                         OakSapling          -> base
                         SpruceSapling       -> base `setBit` 0
                         BirchSapling        -> base `setBit` 1
                         DroppedJungleLeaves -> base `setBit` 0 `setBit` 1
                         FixedJungleLeaves   -> error "The value FixedJungleLeaves can't be set."
               byte  = case count of
                         0 -> base'
                         1 -> base' `setBit` 2
                         2 -> base' `setBit` 3
                         3 -> base' `setBit` 2 `setBit` 3
                         _ -> error $ "A value of " ++ show count ++ " cannot be set. Only 2 bits are available"
           in byte
                         
-- | Convert a Byte to a LiquidData
--   . 0x0 is a full block. Water goes up to 0x7, 
--   . Lava goes up to 0x6 (using the steps 0x0, 0x2, 0x4 and 0x6). 
--   . If bit 0x8 is set, this liquid is "falling" and only spreads downward. 
instance Convertion Int8 LiquidData where
    to x = let val  = sumBits x [0..3]
               fall = x `testBit` 7
               val' = if val `mod` 2 == 0 then val else error $ "Invalid step for liquid. Should be a multiple of 2"
           in if val' == 0
                 then LiquidFull fall
                 else LiquidStep val' fall  
                 
    from (LiquidFull      fall) = if fall then 0 `setBit` 7 else 0
    from (LiquidStep step fall) = let base  = 0 :: Int8
                                      base' = case step of
                                                0 -> base
                                                2 -> base `setBit` 1
                                                4 -> base `setBit` 2
                                                6 -> base `setBit` 1 `setBit` 2
                                                _ -> error $ "Invalid step for liquid. Should be a multiple of 2"
                                  in if fall then base' `setBit` 7 else base'

-- | Convert a Byte to a WoodData
--   . Value 	Description
--   . 	   0 	Oak wood
--   . 	   1 	Pine/Spruce wood
--   . 	   2 	Birch wood
--   . 	   3 	Jungle wood 
instance Convertion Int8 WoodData where
    to x = case x of
             0 -> OakWood
             1 -> PineWood
             2 -> BirchWood
             3 -> JungleWood
             _ -> error "Invalid wood data. expected value between 0 and 3"
             
    from OakWood    = 0
    from PineWood   = 1
    from BirchWood  = 2
    from JungleWood = 3

-- | Convert a Byte to a LeavesData
--   . If bit 0x4 is set, the leaves are permanent and will never decay. 
--   . This bit is set on player-placed leaf blocks and overrides the meaning of bit 0x8.
--   . 
--   . If bit 0x8 is set, the leaves will be checked for decay. 
--   . The bit will be cleared after the check if the leaves do not decay. 
--   . The bit will be set again whenever a block adjacent to the leaves is changed.
--   . Value   Description
--   .     0 	 Oak leaves
--   .     1 	 Pine/Spruce leaves
--   .     2 	 Birch leaves
--   .     3 	 Jungle leaves 
instance Convertion Int8 LeavesData where
    to x = let decay = if x `testBit` 3 then Permanent else Decays
               check = x `testBit` 7
           in case x `sumBits` [0..2] of
                0 -> OakLeaves    decay check
                1 -> PineLeaves   decay check
                2 -> BirchLeaves  decay check
                3 -> JungleLeaves decay check
                
    from x = let base  = case unLd x of
                           Permanent -> 0 `setBit` 3
                           Decays    -> 0
                 base' = if unLc x then base `setBit` 7 else base
             in case x of
                  OakLeaves{}    -> base'
                  PineLeaves{}   -> base' `setBit` 0
                  BirchLeaves{}  -> base' `setBit` 1
                  JungleLeaves{} -> base' `setBit` 0 `setBit` 1

-- | Convert a Byte to a Direction
--   . 0x2: Facing north (for ladders and signs, attached to the north side of a block)
--   . 0x3: Facing south
--   . 0x4: Facing west
--   . 0x5: Facing east 
instance Convertion Int8 Direction where
    to x = case x of
             2 -> ToNorth
             3 -> ToSouth
             4 -> ToWest
             5 -> ToEast
             _ -> error "Invalid Direction. Expected values between 0x2 and 0x5"
    from ToNorth = 2
    from ToSouth = 3
    from ToWest  = 4
    from ToEast  = 5
             

-- | Convert a Byte to a RailData
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
instance Convertion Int8 RailData where
    to x = let powered = x `testBit` 7
               dat     = case x `sumBits` [0,1,2,3] of
                           0 -> FlatGoingNorthSouth
                           1 -> FlatGoingWestEast
                           2 -> TrackAscendingEast
                           3 -> TrackAscendingWest
                           4 -> TrackAscendingNorth
                           5 -> TrackAscendingSouth
                           6 -> CornerConnectEastSouth
                           7 -> CornerConnectWestSouth
                           8 -> CornerConnectWestNorth
                           9 -> CornerConnectEastNorth
                           _ -> error "Invalid Raildata, Expected values between 0x0 and 0x9"
           in RailData powered dat
    from (RailData powered dat) = let base = if powered then 0 `setBit` 7 else 0
                                  in case dat of
                                       FlatGoingNorthSouth    -> base
                                       FlatGoingWestEast      -> base `setBit` 0
                                       TrackAscendingEast     -> base `setBit` 1
                                       TrackAscendingWest     -> base `setBit` 0 `setBit` 1
                                       TrackAscendingNorth    -> base `setBit` 2
                                       TrackAscendingSouth    -> base `setBit` 2 `setBit` 0
                                       CornerConnectEastSouth -> base `setBit` 2 `setBit` 1
                                       CornerConnectWestSouth -> base `setBit` 2 `setBit` 1 `setBit` 0
                                       CornerConnectWestNorth -> base `setBit` 3
                                       CornerConnectEastNorth -> base `setBit` 3 `setBit` 0
                                       _ -> error "Invalid nesting of RailData. Expected a direction instead got RailData"
    from _                      = error "Invalid nesting of the RailData. Expected RailData instead got a direction"

-- | Convert a Byte to a PistonData
--   . The top bit (0x8) is a status bit that determines whether 
--   . the piston is pushed out or not. 1 for pushed out, 0 for retracted.
--   . 
--   . The bottom three bits are a value from 0 to 5, indicating the 
--   . direction of the piston (the direction the piston head is pointing)
--   . 
--   .     0: Down
--   .     1: Up
--   .     2: north
--   .     3: south
--   .     4: west
--   .     5: east 
instance Convertion Int8 PistonData where
    to x = let pressed = x `testBit` 7
               axis = case x `sumBits` [0,1,2] of
                        0 -> Down
                        1 -> Up
                        2 -> North
                        3 -> South
                        4 -> West
                        5 -> East
           in PistonData pressed axis
    
    from (PistonData pressed axis) = let base = if pressed then 0 `setBit` 7 else 0
                                     in case axis of
                                          Down  -> base 
                                          Up    -> base `setBit` 0
                                          North -> base `setBit` 1
                                          South -> base `setBit` 1 `setBit` 0
                                          West  -> base `setBit` 2
                                          East  -> base `setBit` 2 `setBit` 0
                

-- | Convert a Byte to a TallGrassData
instance Convertion Int8 TallGrassData where

-- | Convert a Byte to a BedData
--   . 0x0: Head is pointing south
--   . 0x1: Head is pointing west
--   . 0x2: Head is pointing north
--   . 0x3: Head is pointing east 
--   . 
--   . 0x4: (bit flag) - When 0, the bed is empty. When 1, the bed is occupied.
--   . 0x8: (bit flag) - When 0, the foot of the bed. When 1, the head of the bed. 
instance Convertion Int8 BedData where
    to x = let empty = x `testBit` 3
               bhead = x `testBit` 7
               dir   = case x `sumBits` [0,1,2] of
                         0 -> ToSouth
                         1 -> ToWest
                         2 -> ToNorth
                         3 -> ToEast
           in BedData dir empty bhead
       
    from (BedData dir full bhead) = let base  = if full  then 0 `setBit` 7 else 0
                                        base' = if bhead then base `setBit` 1 else base  
                                    in case dir of
                                         ToSouth -> base'
                                         ToWest  -> base' `setBit` 0
                                         ToNorth -> base' `setBit` 1
                                         ToEast  -> base' `setBit` 0 `setBit` 1

-- | Convert a Byte to a SlabData
--   . 	Value 	Description
--   . 	0x0 	Stone Slab
--   . 	0x1 	Sandstone Slab
--   . 	0x2 	Wooden Slab
--   . 	0x3 	Cobblestone Slab
--   . 	0x4 	Brick Slab
--   . 	0x5 	Stone Brick Slab
--   . 	0x6 	Stone Slab 
instance Convertion Int8 SlabData where
    to x = case x `sumBits` [0,1,2,3] of
             0 -> StoneSlab
             1 -> SandstoneSlab
             2 -> WoodenSlab
             3 -> CobblestoneSlab
             4 -> BrickSlab
             5 -> StoneBrickSlab
             6 -> StoneSlab2
             _ -> error "Invalid Slab data. Range 0..6 expected"
             
    from StoneSlab       = 0
    from SandstoneSlab   = 1
    from WoodenSlab      = 2
    from CobblestoneSlab = 3
    from BrickSlab       = 4
    from StoneBrickSlab  = 5
    from StoneSlab2      = 6
    
-- | Convert a Byte to a StairsData
--   . 0x0: Ascending east
--   . 0x1: Ascending west
--   . 0x2: Ascending south
--   . 0x3: Ascending north 
instance Convertion Int8 StairsData where
    to x = StairsData $ case x `sumBits` [0,1,2,3] of
                          0 -> ToEast
                          1 -> ToWest
                          2 -> ToSouth
                          3 -> ToNorth
                          _ -> error "Invalid Stairs data. Range 0..3 expected"
                      
    from (StairsData ToEast ) = 0
    from (StairsData ToWest ) = 1
    from (StairsData ToSouth) = 2
    from (StairsData ToNorth) = 3

-- | Convert a Byte to a PumpKData
--   . 0x0: Facing south
--   . 0x1: Facing west
--   . 0x2: Facing north
--   . 0x3: Facing east 
instance Convertion Int8 PumpKData where
    to x = PumpKData $ case x `sumBits` [0,1,2,3] of
                         0 -> ToSouth
                         1 -> ToWest
                         2 -> ToNorth
                         3 -> ToEast
                         _ -> error "Invalid Pumpkin data. Range 0..3 expected"
    
    from (PumpKData ToSouth) = 0
    from (PumpKData ToWest ) = 1
    from (PumpKData ToNorth) = 2
    from (PumpKData ToEast ) = 3

-- | Convert a Byte to a Growth
--   . Nether Wart
--   . 
--   . Like Crops, the data value is related to the size of the Nether Wart. There are three distinct visual stages to Nether Wart's growth (the associated values are 0 .. 3)
--   . 
--   . Pumpkin stem and Melon stem
--   . 
--   . Pumpkin and melon stems grow from 0x0 to 0x7. During each stage of growth a part of their model is revealed. In the last stage a stem can spawn a melon or pumpkin next to it on empty farmland. As long as this fruit remains the stem will appear bent towards the fruit. 
instance Convertion Int8 Growth where
    to   = Growth
    from = unGrowth

-- | Convert a Byte to a TorchData
--   . 0x1: Pointing east
--   . 0x2: Pointing west
--   . 0x3: Pointing south
--   . 0x4: Pointing north
--   . 0x5: Standing on the floor 
instance Convertion Int8 TorchData where
    to x = case x `sumBits` [0,1,2,3] of
             1 -> PointEast
             2 -> PointWest
             3 -> PointSouth
             4 -> PointNorth
             5 -> Standing
             _ -> error "Invalid Torch data. Range 1..5 expected"

-- | Convert a Byte to a Spread
instance Convertion Int8 Spread where
    to   = Spread
    from = unSpread

-- | Convert a Byte to a Wetness
instance Convertion Int8 Wetness where
    to x | x >= 0 && x <= 8 = Wetness x
    to _                    = error "Invalid wetness value. Range 0..8 expected"
    
    from (Wetness x) | x >= 0 && x <= 8 = x
    from _                              = error "Invalid wetness value. Range 0..8 expected"

-- | Convert a Byte to a SignData
--   . 0x0: south
--   . 0x1: south-southwest
--   . 0x2: southwest
--   . 0x3: west-southwest
--   . 0x4: west
--   . 0x5: west-northwest
--   . 0x6: northwest
--   . 0x7: north-northwest
--   . 0x8: north
--   . 0x9: north-northeast
--   . 0xA: northeast
--   . 0xB: east-northeast
--   . 0xC: east
--   . 0xD: east-southeast
--   . 0xE: southeast
--   . 0xF: south-southeast 
instance Convertion Int8 SignData where
    to x = case x `sumBits` [0,1,2,3] of
             0  -> PostSouth
             1  -> PostSouth_SouthWest
             2  -> PostSouthWest
             3  -> PostWest_SouthWest
             4  -> PostWest
             5  -> PostWest_NorthWest
             6  -> PostNorthWest
             7  -> PostNorth_NorthWest
             8  -> PostNorth
             9  -> PostNorth_NorthEast
             10 -> PostNorthEast
             11 -> PostEast_NorthEast
             12 -> PostEast
             13 -> PostEast_SouthEast
             14 -> PostSouthEast
             15 -> PostSouth_SouthEast
             _  -> error "Invalid sign data. Range 0..15 expected"
             
    from PostSouth           = 0
    from PostSouth_SouthWest = 1
    from PostSouthWest       = 2
    from PostWest_SouthWest  = 3
    from PostWest            = 4
    from PostWest_NorthWest  = 5
    from PostNorthWest       = 6
    from PostNorth_NorthWest = 7
    from PostNorth           = 8
    from PostNorth_NorthEast = 9
    from PostNorthEast       = 10
    from PostEast_NorthEast  = 11
    from PostEast            = 12
    from PostEast_SouthEast  = 13
    from PostSouthEast       = 14
    from PostSouth_SouthEast = 15

-- | Convert a Byte to a Color
--   . These values specify the color of the wool. 
--   . This data is stored in block metadata for placed wool, 
--   . and as the "damage" for wool in the inventory.
--   .  Dec   Hex 	Description
--   . 	0 	  0x0 	Regular wool (white)
--   . 	1 	  0x1 	Orange
--   . 	2 	  0x2 	Magenta
--   . 	3 	  0x3 	Light Blue
--   . 	4 	  0x4 	Yellow
--   . 	5 	  0x5 	Lime
--   . 	6 	  0x6 	Pink
--   . 	7 	  0x7 	Gray
--   . 	8 	  0x8 	Light Gray
--   . 	9 	  0x9 	Cyan
--   . 	10 	  0xA 	Purple
--   . 	11 	  0xB 	Blue
--   . 	12 	  0xC 	Brown
--   . 	13 	  0xD 	Green
--   . 	14 	  0xE 	Red
--   . 	15 	  0xF 	Black 
instance Convertion Int8 Color where
    to x = case x `sumBits` [0,1,2,3] where
             0  -> White
             1  -> Orange
             2  -> Magenta
             3  -> LightBlue
             4  -> Yellow
             5  -> Lime
             6  -> Pink
             7  -> Gray
             8  -> LightGray
             9  -> Cyan
             10 -> Purple
             11 -> Blue
             12 -> Brown
             13 -> Green
             14 -> Red
             15 -> Black

-- | Convert a Byte to a DoorData
instance Convertion Int8 DoorData where

-- | Convert a Byte to a RepeaterData
instance Convertion Int8 RepeaterData where

-- | Convert a Byte to a Bool
instance Convertion Int8 Bool where

-- | Convert a Byte to a ButtonData
instance Convertion Int8 ButtonData where

-- | Convert a Byte to a CactusData
instance Convertion Int8 CactusData where

-- | Convert a Byte to a SugarData
instance Convertion Int8 SugarData where

-- | Convert a Byte to a JukeboxData
instance Convertion Int8 JukeboxData where

-- | Convert a Byte to a LeverData
instance Convertion Int8 LeverData where

-- | Convert a Byte to a Level
instance Convertion Int8 Level where

-- | Convert a Byte to a TrapDoorData
instance Convertion Int8 TrapDoorData where

-- | Convert a Byte to a SilverFishData
instance Convertion Int8 SilverFishData where

-- | Convert a Byte to a StoneData
instance Convertion Int8 StoneData where

-- | Convert a Byte to a MushroomData
instance Convertion Int8 MushroomData where

-- | Convert a Byte to a BrewingData
instance Convertion Int8 BrewingData where

-- | Convert a Byte to a CauldronData
instance Convertion Int8 CauldronData where

-- | Convert a Byte to a PortalFrameData
instance Convertion Int8 PortalFrameData where
