{-# LANGUAGE ExistentialQuantification #-}

module Moonbase.Preferred
    ( Mimetypes(..)
    , mime
    , Preferred
    , withPreferred
    , (==>)
    , makePreferred
    , userMimeApps
    , mimeImagePng
    , mimeImageJpg
    , mimeImageSvg
    , mimeImages
    , Application(..)
    , app
    , appWith
    ) where 

import Control.Applicative
import Control.Monad.State

import System.Directory
import System.FilePath.Posix
import System.Environment.XDG.BaseDir
import System.Environment.XDG.DesktopEntry hiding (Application)
import System.Environment.XDG.MimeApps

import Data.Monoid
import Data.Maybe
import Data.Foldable

import qualified Data.Map as M

import Moonbase
import Moonbase.Util.Application

data Mimetypes = Mimetypes [String]

instance Monoid Mimetypes where
    mempty = Mimetypes []
    mappend (Mimetypes a) (Mimetypes b) = Mimetypes $ a ++ b

mime :: String -> Mimetypes
mime t = Mimetypes [t]

withPreferred :: forall a. (Executable a) => [(Mimetypes, a)] -> Moonbase ()
withPreferred prefs = modify (\rt -> rt { preferred = value })
  where
      value = if null prefs
                 then Nothing
                 else (Just $ makePreferred prefs)

(==>) :: forall a. (Executable a) => Mimetypes -> a -> (Mimetypes, a)
mime ==> exec = (mime, exec)

makePreferred :: forall a. (Executable a) => [(Mimetypes, a)] -> Preferred
makePreferred prefs = Preferred $ genMap prefs M.empty

genMap :: forall a. (Executable a) => [(Mimetypes, a)] -> M.Map String a -> M.Map String a
genMap ((Mimetypes xt, exec) : xs) m = genMap xs $ addEachMime exec xt m
  where
      addEachMime exec (x:xs) m = addEachMime exec xs $ M.insert x exec m

userMimeApps :: IO FilePath
userMimeApps
    = do
        dir <- getUserDataDir
        return $ dir </> "applications" </> "mimeapps.list"

setPreferred' :: Moonbase ()
setPreferred' = do
    rt       <- get
    mimeApps <- loadMimeApps'
    case preferred rt of
         Nothing    -> return ()
         Just prefs -> do
             path     <- liftIO $ userMimeApps
             mimeApps <- update mimeApps prefs
             liftIO $ saveMimeApps path mimeApps

    where
        update :: MimeApps -> Preferred -> Moonbase MimeApps
        update apps (Preferred m) = foldlMWithKey (updateMime ) apps m

        foldlMWithKey f z = foldlM (\z' (k,v) -> f z' k v) z . M.toAscList
            


        desktopFileName :: (Executable a) => a -> Moonbase FilePath
        desktopFileName exec = do
            userDir <- liftIO $ getUserDataDir
            return $ userDir </> "applications" </> execGetName exec <.> "desktop"

        updateMime :: (Executable a) => MimeApps -> String -> a -> Moonbase MimeApps
        updateMime mimeApps mime exec = do
            entryExists <- liftIO $ findEntry (execGetName exec)

            when (isNothing entryExists) $ do
                   path <- desktopFileName exec
                   liftIO $ saveEntry $
                     newBasicApplication path (execGetName exec) (execGetPath exec)

            return $ addDefault mime (execGetName exec <.> "desktop") mimeApps


loadMimeApps' :: Moonbase MimeApps
loadMimeApps' = do
    dir <- io getUserDataDir
    io $ createDirectoryIfMissing True (dir ++ "/applications")

    exists <- io $ doesFileExist (dir ++ "/applications/mimeapps.list")

    if exists 
       then do
           push (Info "Loading mimeapps file...")
           io (loadMimeApps $ dir ++ "/applications/mimeapps.list")
           else
           push (Info "MimeApps doesn't exists: creating newone")
           >> return newMimeApps  

mimeImagePng = Mimetypes ["image/png"]
mimeImageJpg = Mimetypes ["image/jpeg", "image/jpg", "image/pjpg"]
mimeImageSvg = Mimetypes ["image/svg+xml"]

mimeImages = mimeImagePng <> mimeImageJpg <> mimeImageSvg


mimeVideoMkv  = Mimetypes ["video/x-matroska"]
mimeVideoAvi  = Mimetypes ["video/x-msvideo"]
mimeVideoWebm = Mimetypes ["video/webm"]
mimeVideoMp4  = Mimetypes ["video/mp4"]
mimeVideoMpeg = Mimetypes ["video/mpeg"]
mimeVideoOgv  = Mimetypes ["video/ogg"]
mimeVideoH264 = Mimetypes ["video/h264"]

mimeVideos = mimeVideoMkv <> mimeVideoAvi <> mimeVideoWebm <> mimeVideoMp4 <>
             mimeVideoMpeg <> mimeVideoOgv <> mimeVideoH264


mimeTextC = Mimetypes ["text/x-c"]
mimeTextAsm = Mimetypes ["text/x-asm"]
mimeTextJava = Mimetypes ["text/x-java-source"]
mimeTextJavascript = Mimetypes ["text/javascript"]
mimeTextSh         = Mimetypes ["application/x-sh"]



mimeTextTxt = Mimetypes ["text/plain"]


mimeTextHtml = Mimetypes ["text/html"]
mimeTextXml  = Mimetypes ["application/xml"]



mimeArchiveGz = Mimetypes ["application/x-gzip"]
mimeArchiveZip = Mimetypes ["application/zip"]
mimeArchiveXz  = Mimetypes ["application/x-xz"]
mimeArchiveTar = Mimetypes ["application/x-tar"]
mimeArchiveRar = Mimetypes ["application/x-rar-compressed"]
mimeArchiveDeb = Mimetypes ["application/x-debian-package"]
mimeArchiveCpiio = Mimetypes ["application/x-cpio"]
mimeArchiveBzip  = Mimetypes ["application/x-bzip", "application/x-bzip2"]
mimeArchiveApk   = Mimetypes ["application/vnd.android.package-archive"]

mimePdf = Mimetypes ["application/pdf"]

mimeTorrent = Mimetypes ["application/x-bittorrent"]

mimeOpenDocuments = Mimetypes [ "application/vnd.oasis.opendocument.chart"
                              , "application/vnd.oasis.opendocument.chart-template"
                              , "application/vnd.oasis.opendocument.database"
                              , "application/vnd.oasis.opendocument.formula"
                              , "application/vnd.oasis.opendocument.formula-template"
                              , "application/vnd.oasis.opendocument.graphics"
                              , "application/vnd.oasis.opendocument.graphics-template"
                              , "application/vnd.oasis.opendocument.image"
                              , "application/vnd.oasis.opendocument.image-template"
                              , "application/vnd.oasis.opendocument.presentation"
                              , "application/vnd.oasis.opendocument.presentation-template"
                              , "application/vnd.oasis.opendocument.spreadsheet"
                              , "application/vnd.oasis.opendocument.spreadsheet-template"
                              , "application/vnd.oasis.opendocument.text"
                              , "application/vnd.oasis.opendocument.text-master"
                              , "application/vnd.oasis.opendocument.text-template"
                              , "application/vnd.oasis.opendocument.text-web" ]






















{-
data Application = AppEntry DesktopEntry
                 | AppName  String

class Executeable a where
    getExecPath :: a -> Maybe String

instance Executeable Application where
    getExecPath (AppEntry entry) = getExec entry
    getExecPath (AppName app)    = Just app

data Preferred = forall a. (Executeable a) => Preferred Name a


type MimeType = String

app :: MimeType -> String -> (String, Preferred)
app mime name = (mime, AppName name)

setPreferred :: Moonbase ()
setPreferred   
    = set =<< loadMimeApps'
    where
        fromPreferred (AppName n) = n ++ ".desktop"
        fromPreferred (Entry e) = getName e ++ ".desktop"
        update a = M.foldlWithKey updateMime a <$> (preferred <$> get)
        updateMime a m n = addDefault m (fromPreferred n) a
        set a = do
            dir <- io userMimeApps
            up <- update a
            io $ saveMimeApps dir up

userMimeApps :: IO FilePath
userMimeApps
    = do
        dir <- getUserDataDir
        return $ dir ++ "/applications/mimeapps.list"


loadMimeApps' :: Moonbase MimeApps
loadMimeApps'
    = do
        dir <- io getUserDataDir
        io $ createDirectoryIfMissing True (dir ++ "/applications")

        exists <- io $ doesFileExist (dir ++ "/applications/mimeapps.list")

        if exists 
            then do
                push (Info "Loading mimeapps file...")
                io (loadMimeApps $ dir ++ "/applications/mimeapps.list")
            else
                push (Info "MimeApps doesn't exists: creating newone")
                >> return newMimeApps 



    
-} 
