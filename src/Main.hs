{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import Manager

version = "v0.1"

data Args = Init
          | Add    
            { silent :: Bool
            , files  :: [FilePath]
            }
          | Remove 
            { silent :: Bool
            , files  :: [FilePath]
            }
          | List
          | Update 
          | Clean 
          | Wipe
            deriving (Show, Data, Typeable)

initT  = Init
add    = Add    
  { 
    files = def 
      &= args 
      &= typ "files..."
  , silent = def
      &= name "s"
      &= help "Silent: add to tracking without copying file or\
             \ cleaning/updating tracker." 
  }
    &= help "Adds files to tracking, copying the file into the destination." 
    
remove = Remove 
  {
    files = def 
      &= args
  , silent = def
      &= name "s"
      &= help "Silent: remove from tracking without removing file or\
             \ cleaning/updating tracker." 
  }
    &= help "Removes files from tracking, then cleans and updates the tracker."
list   = List   &= help "Lists files tracked by the manager and their names."
update = Update &= help "Grabs updated copies of tracked files."
clean  = Clean  &= help "Deletes the current set of files cloned in the tracker."
wipe   = Wipe   &= help "Removes .dotmanage, deleting the tracker."

-- helpm  = Help   &= help "Get help."
mode = 
  modes [initT, add, remove, list, update, clean, wipe] 
  &= program "dotmanage"
  &= summary ("dotmanage " ++ version)
  &= help "Simple manager for keeping copies of files that integrates\
             \ with version control, similar to symlinks but with cloned\
             \ files. Initialize a tracker in a target location, add files\
             \ to the tracker, and then update them, grabbing copies of them\
             \ and storing them in the folder. That's it!"

main :: IO ()
main = do
  args >>= run 
  where
    args = getArgs >>= \a ->
      (if null a || a == ["-h"] then withArgs ["--help"] else id)
      $ cmdArgs mode
    run x = case x of 
      Init                   -> tryInitTracker
      Add    {silent, files} -> manageFiles AddFiles    silent files
      Remove {silent, files} -> manageFiles RemoveFiles silent files
      List                   -> listTracker
      Update                 -> updateTracker
      Clean                  -> cleanTracker
      Wipe                   -> wipeTracker