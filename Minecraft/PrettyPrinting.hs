{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.PrettyPrinting
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the basic pretty printing classes for this package along with 
-- basic defaults to be used.
--
-----------------------------------------------------------------------------
module Minecraft.PrettyPrinting
 ( -- * Pretty printing operations
   ppList
 
   -- * Pretty printing classes
 , Pretty(..)
 ) where

import Data.Word
import Data.Int
import Data.List

import Text.PrettyPrint

class Pretty a where
  pp       :: a -> Doc
  
  ppRender :: a -> String
  ppRender = render . pp
  
  ppStyle  :: Style -> a -> String
  ppStyle  = \style -> renderStyle style . pp
  
instance Pretty Int where
  pp = int
  
instance Pretty Int8 where
  pp = int . (fromIntegral :: Int8 -> Int)
  
instance Pretty Int16 where
  pp = int . (fromIntegral :: Int16 -> Int)
  
instance Pretty Int32 where
  pp = int . (fromIntegral :: Int32 -> Int)
  
instance Pretty Int64 where
  pp = int . (fromIntegral :: Int64 -> Int)
  
instance Pretty Float where
  pp = (<> text "f") . float
  
instance Pretty Double where
  pp = double
  
instance Pretty String where
  pp = text
  
instance Pretty a => Pretty [a] where
  pp = brackets . hcat . intersperse comma . map pp
  
  
ppList :: Pretty a => Int -> [a] -> Doc
ppList width = vcat 
             . appendComma 
             . map (hcat . intersperse comma . map pp) 
             . blow'
  where blow' :: [a] -> [[a]]
        blow' xs = case splitAt width xs of
                     (px, []) -> [px]
                     (px, pm) -> px : blow' pm
        appendComma :: [Doc] -> [Doc]
        appendComma []     = []
        appendComma (x:xs) = ((text "[" <> x) : map (comma <>) xs) ++ [text "]"]