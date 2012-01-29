{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.NBT.Data
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Datatype description of the Named Binary Type (NBT) format.
-- Read more at of the format at
-- http://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- Remember that .NBT files are NOT gzipped but .schematic files are.
--
-----------------------------------------------------------------------------
module Minecraft.Format.NBT.Data
 ( -- * Datatype descriptions
   Name(..)
 , NBT_Data(..)
 , NBT
 
   -- * Name creation functions
 , mkName
 ) where

import Data.Int
import Data.Word

import Minecraft.Utils.CaseInSensitive

type NBT = NBT_Data

-- | An abstract representation of a Named and Unnamed structure
data Name 
  = Named { nmStr :: String } -- ^ A named string, e.g. an Explicitly named structure
  | Unnamed                   -- ^ An unnamed structured, only used in NBT_TAG_List
  deriving (Eq, Show)

instance CaseInSensitiveEq Name Name where
    Unnamed    === Unnamed    = True
    (Named s1) === (Named s2) = s1 === s2
    _          === _          = False
    
instance CaseInSensitiveEq String Name where
    s1 === (Named s2) = s1 === s2
    _  === _          = False

instance CaseInSensitiveEq Name String where
    (===) = flip (===)
    
instance CaseInSensitiveEq String NBT where
    a === b = a === nbtName b
  
data NBT_Data 
  = NBT_TAG_End        { nbtName :: Name                      }  -- ^ This tag is used to mark the end of a list.
                                                                 --  .Cannot be named! If type 0 appears where a Named Tag is expected, the name is assumed to be "".
                                                                 --  .(In other words, this Tag is always just a single 0 byte when named, and nothing in all other cases)
  | NBT_TAG_Byte       { nbtName :: Name, nbtByte   :: Int8   }  -- ^ A single signed byte (8 bits)
  | NBT_TAG_Short      { nbtName :: Name, nbtShort  :: Int16  }  -- ^ A signed short (16 bits, big endian)
  | NBT_TAG_Int        { nbtName :: Name, nbtInt    :: Int32  }  -- ^ A signed short (32 bits, big endian)
  | NBT_TAG_Long       { nbtName :: Name, nbtLong   :: Int64  }  -- ^ A signed long (64 bits, big endian)
  | NBT_TAG_Float      { nbtName :: Name, nbtFloat  :: Float  }  -- ^ A floating point value (32 bits, big endian, IEEE 754-2008, binary32)
  | NBT_TAG_Double     { nbtName :: Name, nbtDouble :: Double }  -- ^ A floating point value (64 bits, big endian, IEEE 754-2008, binary64)
  | NBT_TAG_Byte_Array { nbtName :: Name, nbtBArr   :: [Int8] }  -- ^ An array of bytes of unspecified format. The length of this array is <length> bytes
  | NBT_TAG_String     { nbtName :: Name, nbtString :: String }  -- ^ An array of bytes defining a string in UTF-8 format. The length of this array is <length> bytes
  | NBT_TAG_List       { nbtName :: Name, nbtLCount :: Int8
                       , nbtPayload :: [NBT_Data]             } -- ^ A sequential list of Tags (not Named Tags), of type <typeId>. The length of this array is <length> Tags
                                                                -- . Notes:   All tags share the same type
  | NBT_TAG_Compound   { nbtName :: Name
                       , nbtPayload :: [NBT_Data]             } -- ^ A sequential list of Named Tags. This array keeps going until a TAG_End is found.
                                                                --   .         TAG_End end
                                                                --   .Notes:   If there's a nested TAG_Compound within this tag, that one will also have a TAG_End, so simply reading until the next TAG_End will not work.
                                                                --   .         The names of the named tags have to be unique within each TAG_Compound
                                                                --   .         The order of the tags is not guaranteed.
  deriving (Eq, Show)
 
-- | Encode a string as a Name type, Specifically,
--   an empty string should become an unnamed string
mkName :: String -> Name
mkName [] = Unnamed
mkName s  = Named s