-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.NBT.Serialize
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the serializable information for the NBT datatype. This allows
-- one to read and write binary encodings of this format.
--
-----------------------------------------------------------------------------
module Minecraft.Format.NBT.Serialize() where

import Control.Monad

import Data.Serialize
import Data.Word
import Data.Int
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as BS
import qualified Data.Serialize.IEEE754 as IEEE

import Minecraft.Format.NBT.Data


putName False Unnamed = return ()
putName True  Unnamed = put (0 :: Int16)
putName _ (Named str) = do let bstr = B.fromString str
                               len  = fromIntegral (BS.length bstr) :: Int16
                           put len
                           putByteString bstr
  
instance Serialize Name where
  put = putName True
  get = undefined
 
instance Serialize NBT_Data where
  put = putTagged True 
    where putId  = put :: Putter Int8
          putTagged tag x 
             = case x of
                 NBT_TAG_End           _ -> putId' 0
                 NBT_TAG_Byte       nm i -> putId' 1    >> putNm nm >> put i
                 NBT_TAG_Short      nm i -> putId' 2    >> putNm nm >> put i
                 NBT_TAG_Int        nm i -> putId' 3    >> putNm nm >> put i
                 NBT_TAG_Long       nm i -> putId' 4    >> putNm nm >> put i
                 NBT_TAG_Float      nm i -> putId' 5    >> putNm nm >> IEEE.putFloat32be i
                 NBT_TAG_Double     nm i -> putId' 6    >> putNm nm >> IEEE.putFloat64be i
                 NBT_TAG_Byte_Array nm x -> putId' 7    >> putNm nm >> put (len x   :: Int32) >> mapM_ put x
                 NBT_TAG_String   nm str -> let bstr = B.fromString str
                                            in putId' 8 >> putNm nm >> put (fromIntegral (BS.length bstr) :: Int16) >> putByteString bstr
                 NBT_TAG_List     nm i x -> putId' 9    >> putNm nm >> put i                   >> put (len x :: Int32) >> mapM_ (putTagged False) x
                 NBT_TAG_Compound   nm x -> putId' 10   >> putNm nm >> mapM_ (putTagged True) x             >> putId 0
                where putId' = if tag then putId else const (return ())
                      putNm :: Name -> Put
                      putNm  = if tag then put else putName False
                          
          len :: Num c => [a] -> c
          len = fromIntegral . length
    
  get   = do tagType  <- get :: Get Int8
             case tagType of
               0                  -> return $ NBT_TAG_End Unnamed
               x | (x>0 && x<=10) -> do tagNSize <- fmap fromIntegral (get :: Get Int16)
                                        tagName  <- fmap B.toString $ getByteString tagNSize
                                        getFromId tagName tagType
               _                  -> error $ "Invalid tagType " ++ show tagType ++ " found in file."
           
    where getNamedPayload :: Get [NBT_Data] 
          getNamedPayload 
             = do tag <- get
                  case tag of
                    NBT_TAG_End _ -> return []
                    _             -> fmap (tag:) getNamedPayload
                    
          getFromId :: String -> Int8 -> Get NBT_Data
          getFromId str = getFromId' (mkName str)
           where getFromId' :: Name -> Int8 -> Get NBT_Data
                 getFromId' nm 0  = return $ NBT_TAG_End nm
                 getFromId' nm 1  = fmap (NBT_TAG_Byte   nm) get
                 getFromId' nm 2  = fmap (NBT_TAG_Short  nm) get
                 getFromId' nm 3  = fmap (NBT_TAG_Int    nm) get
                 getFromId' nm 4  = fmap (NBT_TAG_Long   nm) get
                 getFromId' nm 5  = fmap (NBT_TAG_Float  nm) IEEE.getFloat32be
                 getFromId' nm 6  = fmap (NBT_TAG_Double nm) IEEE.getFloat64be
                 getFromId' nm 7  = do tagLen <- fmap fromIntegral (get :: Get Int32)
                                       fmap (NBT_TAG_Byte_Array nm) (replicateM tagLen get)
                 getFromId' nm 8  = do tagSSize <- fmap fromIntegral (get :: Get Int16)
                                       str      <- fmap B.toString $ getByteString tagSSize
                                       return $ NBT_TAG_String nm str
                 getFromId' nm 9  = do tagSId  <- get :: Get Int8
                                       tagLen  <- fmap fromIntegral (get :: Get Int32)
                                       tagList <- replicateM tagLen (getFromId "" tagSId)
                                       return $ NBT_TAG_List nm tagSId tagList
                 getFromId' nm 10 = fmap (NBT_TAG_Compound nm) getNamedPayload