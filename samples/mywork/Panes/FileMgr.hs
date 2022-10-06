{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panes.FileMgr
  (
    FileMgrPane
  , initFileMgr
  , myProjectsL
  )
where

import           Brick hiding ( Location )
import           Brick.Panes
import           Brick.Widgets.Center
import qualified Brick.Widgets.Core as BC
import           Brick.Widgets.FileBrowser
import qualified Control.Exception as X
import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson ( ToJSON, FromJSON, decode, encode )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import           Lens.Micro
import           Lens.Micro.Extras ( view )
import           System.Directory ( doesDirectoryExist )

import           Defs


data FileMgrPane

instance Pane WName MyWorkEvent FileMgrPane where
  data (PaneState FileMgrPane MyWorkEvent) =
    FB { fB :: Maybe (FileBrowser WName)
         -- ^ A Nothing value indicates the modal is not currently active
       , myProjects :: Projects
         -- ^ Current loaded set of projects
       , newProjects :: Bool
         -- ^ True when myProjects has been updated; clear this via updatePane
       }
  type (InitConstraints FileMgrPane s) = ()
  type (DrawConstraints FileMgrPane s WName) = ( HasFocus s WName )
  type (EventConstraints FileMgrPane e) = ( HasFocus e WName )
  initPaneState _ = FB Nothing (Projects mempty) False
  drawPane ps gs = drawFB gs <$> fB ps
  focusable _ ps = case fB ps of
                     Nothing -> mempty
                     Just _ -> Seq.fromList [ WFBrowser, WFSaveBtn ]
  handlePaneEvent bs ev ts =
    let isSearching = maybe False fileBrowserIsSearching (ts^.fBrowser)
    in case ev of
      Vty.EvKey Vty.KEsc [] | not isSearching -> return $ ts & fBrowser .~ Nothing
      _ -> case bs^.getFocus of
             Focused (Just WFBrowser) -> handleFileLoadEvent ev ts
             Focused (Just WFSaveBtn) -> handleFileSaveEvent ev ts
             _ -> return ts
  type (UpdateType FileMgrPane) = Bool
  updatePane newFlag ps = ps { newProjects = newFlag }



fBrowser :: Lens' (PaneState FileMgrPane MyWorkEvent) (Maybe (FileBrowser WName))
fBrowser f ps = (\n -> ps { fB = n }) <$> f (fB ps)

myProjectsL :: Lens' (PaneState FileMgrPane MyWorkEvent) Projects
myProjectsL f wc = (\n -> wc { myProjects = n }) <$> f (myProjects wc)


instance ( PanelOps FileMgrPane WName MyWorkEvent panes MyWorkCore
         , HasProjects (PaneState FileMgrPane MyWorkEvent)
         )
  => HasProjects (Panel WName MyWorkEvent MyWorkCore panes) where
  getProjects = getProjects . view (onPane @FileMgrPane)

instance HasProjects (PaneState FileMgrPane MyWorkEvent) where
  getProjects ps = (newProjects ps, myProjects ps)


drawFB :: DrawConstraints FileMgrPane drawstate WName
       => drawstate -> FileBrowser WName -> Widget WName
drawFB ds b =
  let width = 70
      fcsd = ds^.getFocus.to focused
      browserPane fb =
        let hasFocus = fcsd == Just WFBrowser
        in vLimitPercent 55 $ hLimitPercent width
           $ titledB hasFocus "Choose a file"
           $ renderFileBrowser hasFocus fb
      helpPane =
        padTop (BC.Pad 1) $ hLimitPercent width $ vBox
        [ hCenter $ txt "Up/Down: select"
        , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
        , hCenter $ txt "Enter: change directory or select file"
        , hCenter $ txt "Space: change directory"
        , hCenter $ txt "TAB: select Save option"
        , hCenter $ txt "ESC: quit"
        ]
      errDisplay fb = case fileBrowserException fb of
                        Nothing -> emptyWidget
                        Just e -> hLimitPercent width
                                  $ withDefAttr (attrName "Error")
                                  $ strWrap
                                  $ X.displayException e
      savePane = (if fcsd == Just WFSaveBtn
                  then withAttr (attrName "Selected")
                  else id)
                 $ str "[SAVE]"
  in centerLayer (browserPane b <=> errDisplay b <=> savePane <=> helpPane)


handleFileLoadEvent :: Vty.Event
                    -> PaneState FileMgrPane MyWorkEvent
                    -> EventM WName es (PaneState FileMgrPane MyWorkEvent)
handleFileLoadEvent ev ts =
  case ts^.fBrowser of
    Just fb -> do
      b <- nestEventM' fb $ handleFileBrowserEvent ev
      let selectFile =
            case fileBrowserCursor b of
              Nothing -> return $ ts & fBrowser .~ Just b  -- navigation
              Just f ->
                let fp = fileInfoFilePath f
                in liftIO $ doesDirectoryExist fp >>= \e ->
                  if e
                  then return $ ts & fBrowser .~ Just b
                  else do newprjs <- decode <$> liftIO (BS.readFile fp)
                          -- Setting fBrowser to Nothing: exit from modal
                          case newprjs of
                            Nothing -> return $ ts & fBrowser .~ Nothing
                            Just prjs -> return $ (ts { newProjects = True})
                                         & fBrowser .~ Nothing
                                         & myProjectsL .~ prjs
      case ev of
        Vty.EvKey Vty.KEnter [] -> selectFile
        Vty.EvKey (Vty.KChar ' ') [] -> selectFile
        _ -> return $ ts & fBrowser .~ Just b
    Nothing -> return ts  -- shouldn't happen


handleFileSaveEvent :: Vty.Event
                    -> PaneState FileMgrPane MyWorkEvent
                    -> EventM WName es (PaneState FileMgrPane MyWorkEvent)
handleFileSaveEvent _ ts =
  case fileBrowserCursor =<< ts^.fBrowser of
    Nothing -> return ts
    Just f -> let fp = fileInfoFilePath f
              in liftIO (doesDirectoryExist fp) >>= \case
                    True -> return ts -- TODO: show error
                    False -> do liftIO $ BS.writeFile fp (encode $ myProjects ts)
                                return ts


instance ToJSON Projects
instance ToJSON Project
instance ToJSON Role
instance ToJSON Language
instance ToJSON Location
instance ToJSON Note

instance FromJSON Projects
instance FromJSON Project
instance FromJSON Role
instance FromJSON Language
instance FromJSON Location
instance FromJSON Note
-- deriving via Generically Note instance ToJSON Note


initFileMgr :: IO (PaneState FileMgrPane MyWorkEvent)
initFileMgr = do
  fb <- newFileBrowser selectNonDirectories WFBrowser Nothing
  return $ initPaneState fb & fBrowser .~ Just fb
