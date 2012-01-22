-- | The @TestSupport.TestRunner@ module is used to automatically run unit tests.
--
-- Design here is by convention: we will look for tests in the "Test" subdirectory of this package only,
-- and tests are assumed to be NBT format files source files (.nbt, .dat or .schematic) whose filenames start with "Test".
-- Each is assumed to contain its own @main@ function, and to be runnable as an independent application
-- without any command line arguments.
--
-- TestRunner currently uses runhaskell only to execute tests, which it assumes is already on the system path.
module Tests.TestRunner ( runLocalTest, runLocalDebugTest ) where

import Tests.FindFiles
--import Text.Regex.Posix
import System.Cmd (system)
import System.FilePath
import System.Exit
import Data.List

-- | Run and test all the files in Tests\src
runLocalTest :: (String -> IO ExitCode) -> IO ()   
runLocalTest action = runTests "Tests/Src/" action False

-- | Run and test all the files in Tests\src with debugging enabled
runLocalDebugTest :: (String -> IO ExitCode) -> IO ()   
runLocalDebugTest action = runTests "Tests/Src/" action True

-- | Automatically runs tests in the package.
--
-- runTests looks for all Haskell source files in the given subdirectory whose filename starts with \"Test\".
-- These are assumed to be the tests.  Each is assumed to contain its own @main@ function, and to be runnable
-- as an independent application without command line arguments.
--
-- Note that no provision is currently made for tests that hang, or for tests that return some status code.
-- Just your basic stomp-through for now.
runTests :: FilePath -- ^ the starting directory to search in
         -> (String -> IO ExitCode) -- ^ program to run
         -> Bool -> IO ()
runTests startPath action db
    = do files <- findFiles startPath isTest
         mapM_ (\f -> runTest f action db) files
           where isTest :: FilePath -> Bool
                 isTest fpath = let filename = (takeFileName fpath)
                                in "Test_" `isPrefixOf` filename && any (`isSuffixOf` filename) [".dat", ".nbt", ".schematic"]

-- | Runs a test. The test is assumed to contain its own @main@ function, and to be runnable as an independent
-- application without command line arguments.
--
-- This uses runhaskell to execute tests, which it assumes is already on the system path.
--
-- /TODO:/ get Cabal to require and configure the location of runhaskell for us?
runTest :: FilePath -- ^ the path of the source file to run
        -> (String -> IO ExitCode) -- ^ program to run
        -> Bool -> IO ()
runTest test action db
    = do putStrLn ( "[Running test: " ++ test ++ "]" ) 
         exit <- action test
         case exit of 
           ExitSuccess   -> return ()
           ExitFailure _ -> exitFailure
