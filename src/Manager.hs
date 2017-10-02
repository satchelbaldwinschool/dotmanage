{-# LANGUAGE OverloadedStrings #-}
module Manager where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Char
import           Data.List
import           Data.String.Utils
import           GHC.Exts             ()
import           System.Directory
import           System.Exit

data Tracker = Tracker
  { names :: [String]
  , paths :: [FilePath]
  }

data FileCommand = AddFiles | RemoveFiles

type RecordPair = (String, FilePath)

instance ToJSON Tracker where
  toJSON tracker = object
    [ "names" .= names tracker
    , "paths" .= paths tracker
    ]

instance FromJSON Tracker where
  parseJSON = withObject "tracker" $ \o ->
    Tracker <$> o .: "names" <*> o .: "paths"

filename :: String
filename = ".dotmanage"

trimRelative :: FilePath -> FilePath
trimRelative file = if take 2 file == "./" then drop 2 file else file

toRelativeNotation :: FilePath -> IO FilePath
toRelativeNotation path =
  getHomeDirectory >>= \d -> return $ replace d "~" path

toAbsoluteNotation :: FilePath -> IO FilePath
toAbsoluteNotation path =
  getHomeDirectory >>= \d -> return $ replace "~" d path

filepathToName :: FilePath -> String
filepathToName = 
  map toLower 
    . replace " " "-" 
    . replace "/" "--" 
    . replace "~" "home"
    . strip

trackerDoesntExist :: IO ()
trackerDoesntExist = putStrLn "Tracker not initialized in the current folder."

getTracker :: IO FilePath
getTracker = (++ ("/" ++ filename)) <$> getCurrentDirectory

getTrackerContents :: IO Tracker
getTrackerContents = do
  tc <- getTracker >>= B.readFile
  let r = case decodeStrict tc of
        Just x -> x
        _      -> Tracker {names = [], paths = []}
  return r

writeTrackerContents :: Tracker -> IO ()
writeTrackerContents c = flip LB.writeFile (encode (toJSON c)) =<< getTracker

checkForTracker :: IO Bool
checkForTracker = getTracker >>= doesFileExist

tryInitTracker :: IO ()
tryInitTracker =
  checkForTracker >>= \result ->
    if result
      then alreadyExists
      else initTracker
  where
    initTracker =
      createEmptyTracker $ encode Tracker {names = [], paths = []}
    alreadyExists =
      putStrLn "ERROR: Tracker already initialized here." >>
      exitSuccess
    createEmptyTracker :: LB.ByteString -> IO ()
    createEmptyTracker = (getTracker >>=) . flip LB.writeFile

manageFiles :: FileCommand -> Bool -> [FilePath] -> IO ()
manageFiles command silent fpaths =
  checkForTracker >>= \t -> if t 
    then do
      contents <- getTrackerContents
      rpaths <-  mapM (toRelativeNotation . trimRelative) =<< filterM doesFileExist fpaths
      let rnames = map filepathToName rpaths
      zipWithM_ (check command contents) rnames rpaths
      writeTrackerContents $ modifyTracker command contents (zip rnames rpaths)
      unless silent updateTracker
    else
      trackerDoesntExist
  where
    modifyTracker :: FileCommand -> Tracker -> [RecordPair] -> Tracker
    modifyTracker AddFiles contents rpairs =
      foldr (flip addSingle) contents rpairs
    modifyTracker RemoveFiles contents rpairs =
      foldr (flip removeSingle) contents rpairs

    addSingle :: Tracker -> RecordPair -> Tracker
    addSingle tracker pair =
      let name = fst pair
          path = snd pair
      in 
        if notElem name (Manager.names tracker) &&
           notElem path (Manager.paths tracker)  
          then
            Tracker {
              names = Manager.names tracker ++ [name],
              paths = Manager.paths tracker ++ [path]
            }
          else tracker

    removeSingle :: Tracker -> RecordPair -> Tracker
    removeSingle tracker pair =
      let name = fst pair
          path = snd pair
      in
        if elem name (Manager.names tracker) &&
           elem path (Manager.paths tracker)
          then
            Tracker {
              names = delete name (Manager.names tracker),
              paths = delete path (Manager.paths tracker)
            }
          else tracker

    check :: FileCommand -> Tracker -> String -> FilePath -> IO ()
    check AddFiles contents name path = do
      when (elem name $ Manager.names contents)
          (putStrLn $ "Warning: name '" ++ name ++ "' has already been\
          \ added to the tracker. Ignoring.")
      when (elem path $ Manager.paths contents)
        (putStrLn $ "Warning: path '" ++ path ++ "' has already been\
        \ added to the tracker. Ignoring.")
    check RemoveFiles contents name path = do
      when (notElem name $ Manager.names contents)
          (putStrLn $ "Warning: name '" ++ name ++ "' is not in\
          \ the tracker. Ignoring.")
      when (notElem path $ Manager.paths contents)
        (putStrLn $ "Warning: path '" ++ path ++ "' is not in\
        \ the tracker. Ignoring.")

listTracker :: IO ()
listTracker =
  checkForTracker >>= \result ->
    if result then do
        tracker <- getTrackerContents
        if null (Manager.names tracker) || null (Manager.paths tracker) then
          putStrLn "No files added to the tracker."
        else do
          let tnames = Manager.names tracker
          let tpaths = Manager.paths tracker
          let len    = maximum $ map length tnames
          putStrLn $ ("Filename" ++ replicate (len - 8) ' ') ++ " -> " ++ "Relative Path"
          putStrLn "------------------------------"
          zipWithM_ (format_print len) tnames tpaths
      else trackerDoesntExist
  where
    format_print len name path = do
      let padding = len - length name
      putStrLn $ (name ++ replicate padding ' ') ++ " -> " ++ path

updateTracker :: IO ()
updateTracker = 
  checkForTracker >>= \t -> if t
    then do
      contents <- getTrackerContents
      mapM_ fcopy $ zip (Manager.names contents) (Manager.paths contents)
    else trackerDoesntExist
  where
    fcopy :: RecordPair -> IO ()
    fcopy rpair =
      let name = fst rpair
          path = snd rpair
        in
          flip copyFile name =<< toAbsoluteNotation path


cleanTracker :: IO ()
cleanTracker = 
  checkForTracker >>= \e -> if e
    then do
      contents <- getTrackerContents
      mapM_ removeFile $ Manager.names contents
    else trackerDoesntExist

wipeTracker :: IO ()
wipeTracker =
  checkForTracker >>= \e -> if e
    then removeFile filename
    else trackerDoesntExist
