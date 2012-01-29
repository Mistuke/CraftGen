{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Utils.Convertion
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the class definition to allow convertion between
-- two types. It's just a general class to be re-used.
--
-----------------------------------------------------------------------------
module Minecraft.Utils.Convertion 
 (Convertion(..)
 ) where
 
class Convertion a b | a -> b where 
  to   :: a -> b
  from :: b -> a