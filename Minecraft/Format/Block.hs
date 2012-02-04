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
--   As of Beta 1.5, the data value is split in half. The bottom two bits now determine the type of sapling (and thus the eventual tree type), according to the following table:
--   Value 	Description
--   	0 	Oak Sapling
--   	1 	Spruce Sapling
--   	2 	Birch Sapling
--   	3 	Dropped by Jungle Leaves, but otherwise is oak sapling
--   	4+ 	Same as 3, but never dropped
--   
--   The top two bits function as the counter, as in pre-1.5 builds, though obviously there are only four values available in the remaining bits. Either the counter is incremented more slowly to compensate, or trees might just grow more quickly since the update. 
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
--   0x0 is a full block. Water goes up to 0x7, 
--   Lava goes up to 0x6 (using the steps 0x0, 0x2, 0x4 and 0x6). 
--   If bit 0x8 is set, this liquid is "falling" and only spreads downward. 
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
--   Value 	Description
--  	0 	Oak wood
--  	1 	Pine/Spruce wood
--  	2 	Birch wood
--  	3 	Jungle wood 
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
instance Convertion Int8 LeavesData where

-- | Convert a Byte to a Direction
instance Convertion Int8 Direction where

-- | Convert a Byte to a RailData
instance Convertion Int8 RailData where

-- | Convert a Byte to a PistonData
instance Convertion Int8 PistonData where

-- | Convert a Byte to a TallGrassData
instance Convertion Int8 TallGrassData where

-- | Convert a Byte to a BedData
instance Convertion Int8 BedData where

-- | Convert a Byte to a SlabData
instance Convertion Int8 SlabData where

-- | Convert a Byte to a Growth
instance Convertion Int8 Growth where

-- | Convert a Byte to a TorchData
instance Convertion Int8 TorchData where

-- | Convert a Byte to a Spread
instance Convertion Int8 Spread where

-- | Convert a Byte to a Wetness
instance Convertion Int8 Wetness where

-- | Convert a Byte to a SignData
instance Convertion Int8 SignData where

-- | Convert a Byte to a Color
instance Convertion Int8 Color where

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
