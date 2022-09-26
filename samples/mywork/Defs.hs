{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Defs where

import Brick hiding (Location)
import Brick.Focus
import Brick.Panes
import Brick.Widgets.Border
import Data.Text ( Text )
import Data.Time.Calendar
import GHC.Generics ( Generic )
import Lens.Micro
import Lens.Micro.Extras ( view )


newtype Projects = Projects { projects :: [Project] }
  deriving Generic

data Project = Project { name :: Text
                       , role :: Role
                       , description :: Text
                       , language :: Either Text Language
                       , locations :: [Location]
                       }
  deriving Generic

data Role = Author | Maintainer | Contributor | User
  deriving (Show, Enum, Bounded, Eq, Generic)

data Language = Haskell | Rust | C | CPlusPlus | Python | JavaScript
  deriving (Eq, Generic)

data Location = Location { location :: Text
                         , locatedOn :: Maybe Day
                         , notes :: [Note]
                         }
  deriving Generic

data Note = Note { note :: Text
                 , notedOn :: Day
                 }
  deriving Generic


numProjects :: Projects -> Int
numProjects = length . projects


----------------------------------------------------------------------

data MyWorkCore = MyWorkCore { projFile :: FilePath
                             , myWorkFocus :: FocusRing WName
                             }

initMyWorkCore :: MyWorkCore
initMyWorkCore = MyWorkCore { projFile = "projects.json"
                            , myWorkFocus = focusRing [ WProjList, WLocation ]
                            }

coreWorkFocusL :: Lens' MyWorkCore (FocusRing WName)
coreWorkFocusL f c = (\f' -> c { myWorkFocus = f' }) <$> f (myWorkFocus c)


data WName = WSummary | WProjList | WLocation | WNotes | WOps
           | WFileMgr | WFBrowser | WFSaveBtn
           | WPList | WPFilter | WLList
  deriving (Eq, Ord, Show)



-- | Adds a border with a title to the current widget.  First argument is True if
-- the current widget has focus.
titledB :: Bool -> Text -> Widget WName -> Widget WName
titledB fcsd text =
  let ttlAttr = if fcsd then withAttr (attrName "Selected") else id
  in borderWithLabel (ttlAttr $ txt text)


type MyWorkEvent = ()  -- No app-specific event for this simple app


class HasProjects s where
  getProjects :: s -> (Bool, Projects)


instance HasFocus MyWorkCore WName where
  getFocus f s =
    let setFocus jn = case focused jn of
          Nothing -> s
          Just n -> s & coreWorkFocusL %~ focusSetCurrent n
    in setFocus <$> (f $ Focused $ focusGetCurrent (s^.coreWorkFocusL))


class HasSelection s where
  selectedProject :: s -> Maybe Text

instance ( PanelOps Projects WName MyWorkEvent panes MyWorkCore
         , HasSelection (PaneState Projects MyWorkEvent)
         )
  => HasSelection (Panel WName MyWorkEvent MyWorkCore panes) where
  selectedProject = selectedProject . view (onPane @Projects)
