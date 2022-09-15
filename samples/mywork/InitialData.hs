{-# LANGUAGE OverloadedStrings #-}

module InitialData
  (
    initial_projects
  )
where

import           Data.Time.Calendar

import           Defs


initial_projects :: Projects
initial_projects = Projects
  [
    Project
    { name = "brick-panes"
    , description = "Compositional aspect isolation library for Brick-based TUI apps."
    , role = Author
    , language = Right Haskell
    , locations =
      [
        Location
        { location = "/home/kquick/Projects/panes"
        , locatedOn = Nothing
        , notes =
          [ Note
            { note = "Main development location"
            , notedOn = fromGregorian 2022 09 08
            }
          ]
        }
      , Location
        { location = "https://github.com/kquick/brick-panes"
        , locatedOn = Just $ fromGregorian 2022 09 10
        , notes =
          [ Note
            { note = "Published source code VCS location"
            , notedOn = fromGregorian 2022 09 10
            }
          ]
        }
      ]
    }
  
  , Project
    { name = "brick"
    , description = "A declarative terminal user interface library"
    , role = Contributor
    , language = Right Haskell
    , locations =
      [
        Location { location = "/home/kquick/Public/brick"
                 , locatedOn = Just $ fromGregorian 2022 08 28
                 , notes =
                           [ Note
                             { note = "local copy of master"
                             , notedOn = fromGregorian 2022 07 19
                             }
                           , Note
                             { note = "updated to post-1.0 Brick release"
                             , notedOn = fromGregorian 2022 08 28
                             }
                           ]
                 }
      , Location { location = "https://github.com/jtdaugherty/brick"
                 , locatedOn = Just $ fromGregorian 2017 12 27
                 , notes = mempty
                 }
      , Location { location = "https://github.com/kquick/brick"
                 , locatedOn = Nothing
                 , notes =
                   [ Note
                     { note = "Fork for pushing changes to create pull requests \n\
                              \ \n\
                              \ Remember to put changes on a branch!"
                     , notedOn = fromGregorian 2020 10 5
                     }
                   , Note
                     { note = "This repo's master is not always up to date.\n\
                              \ Usually only development branches are pushed, \
                              \ not master."
                     , notedOn = fromGregorian 2020 10 20
                     }
                   ]
                 }
      ]
    }
  ]
