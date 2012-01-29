{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Tests.Test-Parsing
-- Copyright   :  (c) Tamar Christina 2012
-- License     :  BSD3
-- 
-- Maintainer  :  tamar@zhox.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the main entry point for the parsing test interface
--
-----------------------------------------------------------------------------
module Main where

import Data.Serialize
import Tests.TestRunner ( runLocalTest )
import System.Exit
import System.Directory
import qualified Control.Exception as C

import Minecraft.Format.NBT ( loadFile, saveFile )

main = do runLocalTest parse
          exitSuccess
          
parse :: FilePath -> IO ExitCode
parse file = do let action = do src  <- loadFile file
                                let src' = decode (encode src)
                                case src' of
                                  Left str -> putStrLn str >> return (ExitFailure 1)
                                  Right se -> if se == src
                                                 then return ExitSuccess
                                                 else return (ExitFailure 1)
                action `C.catch` (\(x::C.SomeException) -> print x >> (return $ ExitFailure 1))