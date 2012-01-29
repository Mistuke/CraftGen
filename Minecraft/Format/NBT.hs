-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.NBT
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains information for the NBT Minecraft datatype
--
-----------------------------------------------------------------------------
module Minecraft.Format.NBT
 (module Minecraft.Format.NBT.Data
 ,module Minecraft.Format.NBT.Serialize
 
 ,loadFile
 ,saveFile
 ) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL

import Data.Serialize

import Minecraft.ByteString
import Minecraft.PrettyPrinting 
import Minecraft.Format.NBT.Data
import Minecraft.Format.NBT.Serialize 
import Minecraft.Format.NBT.PrettyPrinting 
         
-- | Parse and deserialize a NBT_Data file from disc  
--   If the first byte is 31 (gzip header) then the stream is uncompressed
--   before continuing. If this is not the case we assume the file isn't compressed
--   and try to deserialize it as is.
loadFile :: FilePath -> IO NBT_Data
loadFile file = do content <- BL.readFile file
                   let format = either (putStrLn >> undefined) return
                   if BL.null content
                      then fail "File is empty"
                      else do let fs = case BL.head content of
                                         31 -> GZip.decompress
                                         _  -> id
                              format $ decode $ toStrict $ fs content
     
-- | Serialize and save a NBT_Data file to disc.
--   this also always compresses the files using gzip.     
saveFile :: FilePath -> NBT_Data -> IO ()
saveFile file = BL.writeFile file . GZip.compress . fromStrict . encode
                