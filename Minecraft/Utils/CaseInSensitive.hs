{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Utils.CaseInSensitive
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the class definition to allow case insensitive comparisons
-- between two types. It's just a general class to be re-used.
--
-----------------------------------------------------------------------------
module Minecraft.Utils.CaseInSensitive
 (CaseInSensitiveEq(..)
 ) where
 
import Data.Char
import Data.Function

-- | This class defines two operators to allow us to compare for equality
class CaseInSensitiveEq a b where
    (===) :: a -> b -> Bool
    (=/=) :: a -> b -> Bool
    (=/=) = \a b-> not $ a === b
  
-- | Default instance for strings where both are converted to lowercase
--   before comparison
instance CaseInSensitiveEq String String where
    (===) = (==) `on` (map toLower)
    