-----------------------------------------------------------------------------
-- |
-- Module      :  Minecraft.Format.NBT.PrettyPrinting
-- Copyright   :  (c) Tamar Christina 2011
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains pretty printing information for the NBT Minecraft datatype
--
-----------------------------------------------------------------------------
module Minecraft.Format.NBT.PrettyPrinting
 ( pp
 ) where

import Text.PrettyPrint

import Data.Int

import Minecraft.Format.NBT.Data
import Minecraft.PrettyPrinting

maxWidth = 20 :: Int

instance Pretty Name where
  pp Unnamed   = empty
  pp (Named s) =  parens (doubleQuotes (text s))
  
instance Pretty NBT_Data where
  pp (NBT_TAG_End           _) = empty
  pp (NBT_TAG_Byte       nm i) = text "TAG_Byte"             <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Short      nm i) = text "TAG_Short"            <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Int        nm i) = text "TAG_Int"              <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Long       nm i) = text "TAG_Long"             <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Float      nm i) = text "TAG_Float"            <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Double     nm i) = text "TAG_Double"           <> pp nm <> text ":" <+> pp i
  pp (NBT_TAG_Byte_Array nm x) = vcat [text "TAG_Byte_Array" <> pp nm <> text ":" <+> brackets (pp (length x))
                                      ,text "{"
                                      ,nest 5 (ppList maxWidth x)
                                      ,text "}"
                                      ]
  pp (NBT_TAG_String     nm s) = text "TAG_String"         <> pp nm <> text ":" <+> quotes (text s)
  pp (NBT_TAG_List     nm t x) = vcat [text "TAG_List"     <> pp nm <> text ":" <+> brackets (pp (length x) <+> text "of" <+> lookupName t)
                                      ,text "{"
                                      ,vcat (map (nest 5 . pp) x)
                                      ,text "}"
                                      ]
  pp (NBT_TAG_Compound   nm x) = vcat [text "TAG_Compound" <> pp nm <> text ":" <+> pp (length x)           <+> text "entries"
                                      ,text "{"
                                      ,vcat (map (nest 5 . pp) x)
                                      ,text "}"
                                      ]
 
 
lookupName :: Int8 -> Doc
lookupName = maybe empty text . flip lookup list
  where list :: [(Int8, String)]
        list = [(1 , "TAG_Byte"      )
               ,(2 , "TAG_Short"     )
               ,(3 , "TAG_Int"       )
               ,(4 , "TAG_Long"      )
               ,(5 , "TAG_Float"     )
               ,(6 , "TAG_Double"    )
               ,(7 , "TAG_Byte_Array")
               ,(8 , "TAG_String"    )
               ,(9 , "TAG_List"      )
               ,(10, "TAG_Compound"  )
               ]