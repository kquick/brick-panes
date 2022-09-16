{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Brick hiding ( Location )
import           Brick.Focus
import           Brick.Panes
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Lens
import           Control.Monad ( guard, when )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.List as DL
import           Data.Maybe ( catMaybes )
import           Data.Version ( showVersion )
import           Graphics.Vty ( defAttr, withStyle, defaultStyleMask
                              , bold, reverseVideo, dim
                              , black, white, yellow, red )
import qualified Graphics.Vty as Vty

import           Defs
-- import           InitialData
import           Panes.Location ()
import           Panes.Operations
import           Panes.Projects ()
import           Panes.Summary
import           Panes.FileMgr
import           Paths_brick_panes ( version )


type MyWorkState = Panel WName MyWorkEvent MyWorkCore
                   '[ SummaryPane
                    , OperationsPane
                    , Location
                    , Projects
                    , FileMgrPane
                    ]

main :: IO ()
main = defaultMain myworkApp initialState >> return ()


myworkApp :: App MyWorkState MyWorkEvent WName
myworkApp = App { appDraw = drawMyWork
                , appChooseCursor = showFirstCursor
                , appHandleEvent = handleMyWorkEvent
                , appStartEvent = return ()
                , appAttrMap = const myattrs
                }

myattrs :: AttrMap
myattrs = attrMap defAttr
          [
            (editAttr, white `on` black)
          , (editFocusedAttr, yellow `on` black)

          , (listAttr, defAttr `withStyle` defaultStyleMask)
          , (listSelectedAttr, defAttr `withStyle` bold)
          , (listSelectedFocusedAttr, defAttr `withStyle` reverseVideo)

          , (attrName "disabled", defAttr `withStyle` dim)
          , (attrName "Selected", black `on` yellow)
          , (attrName "Error", fg red)
          ]

initialState :: MyWorkState
initialState = focusRingUpdate myWorkFocusL
               $ addToPanel Never
               $ addToPanel Never
               $ addToPanel WhenFocused
               $ addToPanel WhenFocused
               $ addToPanel (WhenFocusedModal Nothing)
               $ basePanel initMyWorkCore

drawMyWork :: MyWorkState -> [Widget WName]
drawMyWork mws =
  let mainPanes =
        [
          borderWithLabel  (str $ " mywork " <> showVersion version <> " ")
          $ vBox $ catMaybes
          [
            panelDraw @SummaryPane mws
          , Just hBorder
          , Just $ hBox $ catMaybes
            [ hLimitPercent 25
              <$> panelDraw @Projects mws
            , Just vBorder
            , panelDraw @Location mws
            ]
          , Just hBorder
          , panelDraw @OperationsPane mws
          ]
        ]
      allPanes = catMaybes [ panelDraw @FileMgrPane mws
                           ]
                 <> mainPanes
      disableLower = \case
        (m:ls) -> m : (withDefAttr (attrName "disabled") <$> ls)
        o -> o
  in joinBorders . withBorderStyle unicode <$> disableLower allPanes


handleMyWorkEvent :: BrickEvent WName MyWorkEvent -> EventM WName MyWorkState ()
handleMyWorkEvent = \case
    AppEvent _ -> return () -- this app does not use these
    -- Application global actions
    --   * CTRL-q quits
    --   * CTRL-l refreshes vty
    --   * ESC dismisses any modal window
    VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])  -> halt
    VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])  -> do
      vty <- getVtyHandle
      liftIO $ Vty.refresh vty
    VtyEvent (Vty.EvKey (Vty.KFun 1) []) -> do
      fmgr <- liftIO initFileMgr
      modify ((focusRingUpdate myWorkFocusL) . (onPane @FileMgrPane .~ fmgr))
    -- Otherwise, allow the Panes in the Panel to handle the event
    ev -> do proj0 <- gets selectedProject
             get >>= (\s -> handleFocusAndPanelEvents myWorkFocusL s ev) >>= put
             (new,prjs) <- gets getProjects
             let mprj s = do pnm <- selectedProject s
                             guard (Just pnm /= proj0)
                             DL.find ((== pnm) . name) (projects prjs)
             when new $
               modify $ \s -> s
                              & focusRingUpdate myWorkFocusL
                              & onPane @Projects %~ updatePane prjs
                              & onPane @FileMgrPane %~ updatePane False
             modify $ \s ->
                        case mprj s of
                          Just p -> s & onPane @Location %~ updatePane p
                          _ -> s
             modify $ focusRingUpdate myWorkFocusL

myWorkFocusL :: Lens' MyWorkState (FocusRing WName)
myWorkFocusL = onBaseState . coreWorkFocusL
