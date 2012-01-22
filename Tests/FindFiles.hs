module Tests.FindFiles (findFiles, findAllFiles) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

-- concatMapM, partitionM: pull out into monad utility lib? Does some version
-- of these already exist somewhere?
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
 
-- Performs monadic predicate on list and partitions the result into two lists.
-- I don't think we can use partition p xs = (filter p xs, filter (not . p) xs)
-- as an implementation guide since we only want to run the predicate
-- actions once.
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = mapM p xs >>= \bools -> return $ partition' bools xs [] []
    where partition' :: [Bool] -> [a] -> [a] -> [a] -> ([a], [a])
          partition' _ [] yes no = (reverse yes, reverse no)
          partition' (b:bs) (x:xs) yes no = if b then partition' bs xs (x:yes) no
                                            else partition' bs xs yes (x:no)

-- Find all files (recursively) in the given directory matching the predicate f.
findFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFiles path f = (liftM (filter f) . findAllFiles) path

-- Big Assumption: every element that isn't a directory is a file! I assume
-- that the Directory functions treat links reasonably, so that everything
-- falls into these two nice neat buckets.
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do elems <- getElems path
                       (subdirs, files) <- partitionM doesDirectoryExist elems
                       nested_files <- concatMapM findAllFiles subdirs
                       return $ files ++ nested_files
    where
      getElems = liftM filterElems . getDirectoryContents
      filterElems = map addPath . filter notDotOrDotDot
      addPath = (path </>)
      notDotOrDotDot = (`notElem` [".", ".."])
