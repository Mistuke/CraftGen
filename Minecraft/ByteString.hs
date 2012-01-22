-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.ByteString
-- Copyright   :  (c) Tamar Christina 2011
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A few helper functions between lazy and strict ByteStrings
--
-----------------------------------------------------------------------------
module Minecraft.ByteString
 (fromStrict
 ,toStrict
 ) where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Foreign.ForeignPtr
import           Foreign.Ptr

-- see benchmark code at end of mail for the qualified imports

-- |/O(n)/ Convert a strict ByteString into a lazy ByteString.
fromStrict :: B.ByteString -> BL.ByteString
fromStrict = flip BLI.chunk BLI.Empty

-- |/O(n)/ Convert a lazy ByteString into a strict ByteString.
toStrict :: BL.ByteString -> B.ByteString
toStrict lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go  BLI.Empty                   _   = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
        withForeignPtr fp $ \p -> do
            BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
            go r (ptr `plusPtr` l)