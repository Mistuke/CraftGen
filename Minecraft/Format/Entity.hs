{-# LANGUAGE TemplateHaskell, TypeOperators #-}
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

import Minecraft.Format.Block ( Color(..), Block(..), Direction(..) )

import Control.Category
import Data.Int
import Data.Label
import Prelude hiding ((.), id)

type Entities = [Entity]

-- | The top level entity type
data Entity = Entity
    deriving (Eq, Show)

-- | The entity rotation
data Rotation 
  = Rotation { _yaw   :: Float -- ^ The rotation yaw
             , _pitch :: Float -- ^ The rotation pitch
             }
  deriving (Eq, Show)
  
-- | Coordinate position
data Position a
  = Position { _x :: a -- ^ The x-position of the entity
             , _y :: a -- ^ The y-position of the entity
             , _z :: a -- ^ The z-position of the entity
             }
  deriving (Eq, Show)
  
-- | The motion of the entity
data Motion
  = Motion { _dX :: Double -- ^ The x velocity of the entity
           , _dY :: Double -- ^ The y velocity of the entity
           , _dZ :: Double -- ^ The z velocity of the entity
           }
  deriving (Eq, Show)

-- | The shared common based of all entities
--   . Every entity is an unnamed TAG_Compound contained in the Entities list of a chunk file.
--   . The sole exception is the Player entity, stored in level.dat.
--
--   . All entities share this base: 
data BaseEntity
  = BaseEntity { _identity     :: String          -- ^ Entity ID
               , _pos          :: Position Double -- ^ 3 TAG_Doubles describing the current X,Y,Z position of the entity. 
               , _motion       :: Motion          -- ^ 3 TAG_Doubles describing the current dX,dY,dZ velocity of the entity. 
                                                  --     (Note: Falling into the Void appears to set this to ridiculously high speeds. 0,0,0 is no motion.) 
               , _rotation     :: Rotation        -- ^ Two TAG_Floats representing rotation in degrees. 
                                                  --   . TAG_Float[0]: The entity's rotation clockwise around the Y axis (called yaw). 
                                                  --   .               Due west is 0. Can have large values because it accumulates all of the 
                                                  --   .               entity's lateral rotation throughout the game.
                                                  --   . TAG_Float[1]: The entity's declination from the horizon (called pitch). 
                                                  --   .               Horizontal is 0. Positive values look downward. Does not exceed 
                                                  --   .               positive or negative 90 degrees.
               , _fallDistance :: Float           -- ^ Distance the entity has fallen. Larger values cause more damage when the entity lands.
               , _fire         :: Int16           -- ^ Number of ticks until the fire is put out. Negative values reflect how long the entity can stand in fire before burning. 
               , _air          :: Int16           -- ^ How much air the entity has, in ticks. Fills to a maximum of 200 in air, giving 10 seconds submerged before the entity starts to drown, 
                                                  --   and a total of up to 20 seconds before the entity dies. Decreases while underwater. If 0 while underwater, the entity loses 1 health per second. 
               , _onGround     :: Bool            -- ^ How much air the entity has, in ticks. Fills to a maximum of 200 in air, giving 10 seconds submerged before the entity starts to drown, and a total of up to 20 seconds before the entity dies. 
                                                  --   . Decreases while underwater. If 0 while underwater, the entity loses 1 health per second.                                
               }
  deriving (Eq, Show)
  
-- | Additional information for base mob entities
data BaseMob 
   = BaseMob { _baseMob    :: BaseEntity -- ^ The bases for all entities
             , _attackTime :: Int16      -- ^ Number of ticks the entity's "invincibility shield" is lasting after the entity was last struck. 
             , _deathTime  :: Int16      -- ^ Number of ticks the entity has been dead for. Controls death animations.
             , _mobHealth  :: Int16      -- ^ Amount of health the entity has. Players and enemies normally have up to 20 health. Livestock has up to 10 health. 
             , _hurtTime   :: Int16      -- ^ Unknown, maybe time invincible after being hit 
             }
  deriving (Eq, Show)

-- | The mob entity type
data Mob 
   = Blaze       { _mobBase :: BaseMob }
   | CaveSpider  { _mobBase :: BaseMob } 
   | Chicken     { _mobBase :: BaseMob } 
   | Cow         { _mobBase :: BaseMob } 
   | Creeper     { _mobBase :: BaseMob 
                 , _powered :: Bool -- ^ 1 or 0 (true/false) - true if the creeper was struck by lightning (doubles the explosive power). This tag is only written if value is 1 (true). 
                 } 
   | EnderDragon { _mobBase :: BaseMob } 
   | Enderman    { _mobBase :: BaseMob 
                 , _carried :: Block -- ^ Block the enderman is carrying
                 } 
   | Ghast       { _mobBase :: BaseMob } 
   | Giant       { _mobBase :: BaseMob } 
   | LavaSlime   { _mobBase :: BaseMob } 
   | Mob         { _mobBase :: BaseMob } 
   | Monster     { _mobBase :: BaseMob } 
   | MushroomCow { _mobBase :: BaseMob } 
   | Ozelot      { _mobBase :: BaseMob } 
   | Pig         { _mobBase :: BaseMob 
                 , _Saddle  :: Bool -- ^ 1 or 0 (true/false) - true if there is a saddle on the pig. 
                 } 
   | Sheep       { _mobBase :: BaseMob 
                 , _sheared :: Bool  -- ^ 1 or 0 (true/false) - true if the sheep has been shorn. 
                 , _color   :: Color -- ^ 0 to 15 - see wool data values for a mapping to colors.
                 } 
   | Silverfish  { _mobBase :: BaseMob } 
   | Skeleton    { _mobBase :: BaseMob } 
   | Slime       { _mobBase :: BaseMob 
                 , _size    :: Int32 -- ^ The size of the slime
                 } 
   | SnowMan     { _mobBase :: BaseMob } 
   | Spider      { _mobBase :: BaseMob } 
   | Squid       { _mobBase :: BaseMob } 
   | Villager    { _mobBase :: BaseMob } 
   | Wolf        { _mobBase :: BaseMob 
                 , _owner   :: String -- ^ Name of the player that owns this wolf. Empty string if no owner. 
                 , _sitting :: Bool   -- ^ 1 or 0 (true/false) - true if the wolf is sitting. 
                 , _angry   :: Bool   -- ^ 1 or 0 (true/false) - true if the wolf is angry. 
                 } 
   | Zombie      { _mobBase :: BaseMob } 
   | PigZombie   { _mobBase :: BaseMob 
                 , _anger   :: Int16 -- ^ Anger level. Determines the agressivity of the creature towards players. 
                 }
 deriving (Eq, Show)
 
-- ^ Common base for all projectiles
data BaseProjectile
   = BaseProjectile { _baseProjectile :: BaseEntity -- ^ The base for all entities
                    , _posBP          :: Position Int -- ^ Position of the item in the chunk
                    , _inTile         :: Int8         -- ^ For arrows being stuck into blocks. 
                    , _shake          :: Int8         -- ^ The "shake" when arrows hit a block.
                    , _inGround       :: Bool         -- ^ 1 or 0 (true/false) - Unknown 
                    }
 deriving (Eq, Show)
                    
-- | Projectile types
data Projectile
   = Arrow            { _projectileBase :: BaseProjectile }
   | Snowball         { _projectileBase :: BaseProjectile }
   | Egg              { _projectileBase :: BaseProjectile }
   | Fireball         { _projectileBase :: BaseProjectile }
   | SmallFireball    { _projectileBase :: BaseProjectile }
   | ThrownEnderpearl { _projectileBase :: BaseProjectile }
 deriving (Eq, Show)
   
-- | Item entity type
data Item
   = Item     { _baseItem   :: BaseEntity -- ^ Common base for all items
              , _itemHealth :: Int16      -- ^ Starts at 5, and currently only decreases as the item 
                                          --   takes fire damage. When health reaches 0, the item is destroyed.
              , _age        :: Int16      -- ^ The amount of time an item has been "untouched" on the ground. 
                                          --   After 6000 'ticks' (5 minutes [1]) the item is destroyed. 
              , _itemId     :: Int16      -- ^ Item or Block ID
              , _itemDamage :: Int16      -- ^ The amount of wear each item has suffered. 0 means undamaged. 
                                          -- ^ When the Damage exceeds the item's durability, it breaks and disappears.
                                          --   Only tools and armor accumulate damage normally. 
              , _itemCount  :: Int8       -- ^ Number of items stacked in this inventory slot. Any item can be stacked, 
                                          --   including tools, armor, and vehicles. Range is 1-255. Values above 127 
                                          --   are not displayed in-game. 
              , _enchId     :: Int16      -- ^ Id of the Enchantment
              , _enchLvl    :: Int16      -- ^ Level of the Enchantment
              } 
   | Painting { _baseItem :: BaseEntity   -- ^ Common base for all items
              , _dir      :: Direction    -- ^ Direction the painting faces: 0 is east, 1 is north, 2 is west, and 3 is south. 
              , _motive   :: String       -- ^ The name of this Painting's art
              , _tilePos  :: Position Int -- ^ The coordinate of the block the painting is hanging on
              } 
   | XPOrb    { _baseItem   :: BaseEntity -- ^ Common base for all items
              , _itemHealth :: Int16      -- ^ Starts at 5, and currently only decreases as the item takes fire damage. 
                                          --   When health reaches 0, the item is destroyed. 
              , _age        :: Int16      -- ^ The amount of time an orb has been "untouched" on the ground. 
                                          --   After a certain amount of ticks, the orb is destroyed.
              , _value      :: Int16      -- ^ The amount of experience the orb gives when picked up                                          
              } 
 deriving (Eq, Show)
 
-- | Vehicle entity type
data Vehicle
   = Boat     { _baseVehicle :: BaseEntity }
   | Minecart { _baseVehicle :: BaseEntity             -- ^ Common base for all entities
              , _cartType    :: MinecartType           -- ^ The type of the minecraft catt, want je lijkt het zelf niet te weten.
              -- , _cartFurnace :: Maybe MinecraftFurnace -- ^ For carts with a furance there are extra fields
              -- , _cartChest   :: Maybe 
              }
  deriving (Eq, Show)
 
-- | The type of a minecart 
data MinecartType
   = EmptyCart   -- ^ 0 cart with nothing
   | ChestCart   -- ^ 1 cart with a chest
   | FurnaceCart -- ^ 2 cart with a furnace
  deriving (Eq, Show)
         
$(mkLabels [ ''BaseEntity
           , ''Motion
           , ''Position
           , ''Rotation
           , ''BaseMob
           , ''Mob
           , ''BaseProjectile
           , ''Projectile
           , ''Item
           , ''Vehicle
           ])