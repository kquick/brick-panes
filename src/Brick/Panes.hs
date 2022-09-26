{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This package provides an overlay library for Brick that allows
  individual TUI screen areas to be independently developed and then easily
  composed into the overall application.
-}

module Brick.Panes
  (
    -- * Pane Specification
    -- ** Definition and Initialization
    Pane
  , PaneState
  , InitConstraints
  , initPaneState
    -- ** Drawing
  , DrawConstraints
  , drawPane
    -- ** Event Handling
  , EventConstraints
  , EventType
  , DispatchEvent
  , focusable
  , handlePaneEvent
  , updatePane
  , enteredModal
  , exitedModal
  , PanelMode(Normal)
  , PanelTransition
    -- ** Focus management helpers and constraints
  , focus1If
  , HasFocus
  , getFocus
  , Focused(Focused)
  , focused
  , focusRingUpdate
    -- * Panel Specification
    -- ** Definition and Initialization
  , Panel
  , basePanel
  , addToPanel
  , PaneFocus( Always, Never, WhenFocused, WhenFocusedModal
             , WhenFocusedModalHandlingAllEvents
             )
    -- ** Pane and base state access
  , onPane
  , onBaseState
    -- ** Drawing
  , panelDraw
    -- ** Focus and Event management
  , handleFocusAndPanelEvents
    -- ** Access and operations
  , PanelOps(..)
  )
where

import           Control.Applicative ( (<|>) )
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Kind ( Constraint, Type )
import qualified Data.List as L
import           Data.Maybe ( fromMaybe )
import           Data.Sequence ( Seq, (><) )
import qualified Data.Sequence as Seq
import           Data.Void ( Void, absurd )
import           GHC.TypeLits
import qualified Graphics.Vty as Vty

import           Brick
import           Brick.Focus


-- | Class to manage each pane in the Brick TUI.
--
-- Type parameters:
--
--  *  @pane@ = Pane Type, uniquely identifying this pane
--  *  @n@ = Widget type parameter
--  *  @updateType@ = Update type (passed to 'updatePane')
--
-- The 'PaneState' specifies the state that should be stored globally
-- and which provides the primary information for handling this pane
-- (for both draw and event handling operations).
--
-- The 'initPaneState' method is responsible for returning an initial 'PaneState'
-- value (at startup).
--
-- The 'drawPane' method is called to render the pane into a 'Widget' (or Nothing
-- if this Pane should not currently be drawn).  It is passed the 'PaneState' and
-- also a drawing parameter.  The 'DrawConstraints' can be used to specify
-- additional instance requirements for the drawing parameter.  The global
-- application state is often passed as this drawing parameter, but the
-- 'drawPane' method should only perform 'DrawConstraints' operations, along with
-- general Brick drawing operations.
--
-- The 'focusable' method should return the names of the widgets that can be the
-- target of the 'FocusRing' in the current state.  This should always return an
-- empty list if the 'drawPane' returns 'Nothing'.
--
-- The 'handlePaneEvent' method is called to handle an event that has occurred
-- within this Pane.  It should return the updated 'PaneState' in the context of
-- an 'EventM' monadic operation.
--
-- The 'updatePane' method is called with the 'updateType' to perform any
-- updating of the 'PaneState' from the update type data.
class Pane n appEv pane updateType | pane -> n, pane -> updateType where

  -- | State information associated with this pane
  data (PaneState pane appEv)

  -- | Constraints on argument passed to 'initPaneState'.  If there are no
  -- constraints, this may be specified as @()@, or simply omitted because @()@
  -- is the default.
  type (InitConstraints pane initctxt) :: Constraint
  -- | Function called to initialize the internal 'PaneState'
  initPaneState :: (InitConstraints pane i) => i -> PaneState pane appEv

  -- | Constraints on the @drawcontext@ parameter passed to 'drawPane'.
  type (DrawConstraints pane drwctxt n) :: Constraint
  -- | Function called to draw the 'Pane' as a Brick 'Widget', or 'Nothing' if
  -- this 'Pane' should not be drawn at the current time.
  drawPane :: (DrawConstraints pane drawcontext n, Eq n)
           => PaneState pane appEv -> drawcontext -> Maybe (Widget n)

  -- | The constraints that should exist on the 'eventcontext' argment passed to
  -- 'focusable' and 'handlePaneEvent'.
  type (EventConstraints pane evctxt) :: Constraint
  -- | The type of the event argument delivered to 'handlePaneEvent'.  This
  -- should either be 'Vty.Event' or 'BrickEvent', depending on what level of
  -- granularity the 'handlePaneEvent' operates at.
  type (EventType pane n appEv)
  -- | The 'focusable' method is called to determine which Widget targets should
  -- be part of the Brick 'FocusRing'.
  focusable :: (EventConstraints pane eventcontext, Eq n)
            => eventcontext -> PaneState pane appEv -> Seq.Seq n
  -- | Called to handle an 'EventType' event for the 'Pane'.  This is typically
  -- only called when (one of the 'focusable' targets of) the 'Pane' is the focus
  -- of the 'FocusRing'.  It should modify the internal 'PaneState' as
  -- appropriate and make any appropriate changes to properly render the 'Pane'
  -- on the next 'drawPane' call.
  --
  -- Note that this function also receives an eventcontext which it may stipulate
  -- constraints on.  Those constraints should be *read-only* constraints.  This
  -- is especially important when the pane is used as part of a panel: the Panel
  -- itself is passed as the eventcontext, but the panel may not be modified
  -- because the panel event dispatching will overwrite any changes on
  -- completion.
  handlePaneEvent :: (EventConstraints pane eventcontext, Eq n)
                  => eventcontext
                  -> EventType pane n appEv
                  -> PaneState pane appEv
                  -> EventM n es (PaneState pane appEv)
  -- | Function called to update the internal 'PaneState', using the passed
  -- 'updateType' argument.
  updatePane :: updateType -> PaneState pane appEv -> PaneState pane appEv

  -- A set of defaults that allows a minimal instance specification
  type (InitConstraints pane initctxt) = ()
  type (DrawConstraints pane drwctxt n) = ()
  type (EventConstraints pane evctxt) = ()
  type (EventType pane n appev) = Vty.Event  -- by default, handle Vty events
  focusable _ _ = mempty
  handlePaneEvent _ _ = return
  updatePane _ = id


-- | This is a helper function for a Pane with a single Widget name and a
-- conditional focus.  For example, if a widget is always focusable, then it can
-- specify:
--
--  > instance Pane N E ThisPane () where
--  >   ...
--  >   focusable _ = const $ focus1If MyWidgetName True
focus1If :: n -> Bool -> Seq.Seq n
focus1If n b = if b then Seq.singleton n else mempty


-- | This class allows retrieval of the current focused Widget (if any).  This
-- class is frequently specified as one of the constraints for the
-- 'DrawConstraints' or 'EventConstraints' of a 'Pane'.
class HasFocus b n | b -> n where
  -- | Provides a lens from the primary type to the 'Focused' type, which
  -- specifies the current focused element (if any).
  getFocus :: Lens' b (Focused n)
  -- By default, nothing has Focus
  getFocus f x = const x <$> f (Focused Nothing)

-- | This is a newtype to wrap the identification of the current focused element
-- (if any).
newtype Focused n = Focused { focused :: Maybe n
                              -- ^ The current focused element or 'Nothing'.
                            }


-- | The 'DispatchEvent' class is used to determine which type of event to
-- dispatch to a 'Pane' by selecting on the @'EventType' pane n@.  This is used
-- internally in the brick-panes implementation and client code does not need to
-- explicitly specify instances of this class.
class DispatchEvent n appev pane evtype where
  dispEv :: ( Pane n appev pane updateType
            , EventConstraints pane base
            , Eq n
            )
         => EventType pane n appev :~: evtype
         -> base -> BrickEvent n appev -> PaneState pane appev
         -> EventM n es (PaneState pane appev)

instance DispatchEvent n appev pane (BrickEvent n appev) where
  dispEv Refl base ev s = handlePaneEvent base ev s

instance DispatchEvent n appev pane Vty.Event where
  dispEv Refl base ev s = case ev of
    VtyEvent vev -> handlePaneEvent base vev s
    _ -> return s


----------------------------------------------------------------------
-- A Panel is a composite of a number of panes

-- | A Panel is a recursive data sequence of individual 'Pane' elements
-- with a core state.  The core state represents the base state of the
-- Brick application, independent of the various Pane data.  Each 'Pane'
-- has an instance that defines its 'PaneState', which is associated
-- here with a potential Widget name (allowing selected actions; see
-- 'handleFocusAndPanelEvents').
--
-- The 'Panel' type closes over the 'state' type argument, which is used for all
-- three of the 'Pane' constraints ('DrawConstraints', 'EventConstraints', and
-- indirectly the 'InitConstraints'), which means that the same 'state' type must
-- be passed to all three associated Pane methods; a 'Pane' used outside of the
-- 'Panel' container is not constrained in this manner and each method could have
-- a different argument.  For the Panel, the 'state' is typically the Panel
-- "beneath" the current Pane, which is the aggregate of the base state and all
-- Panes added before the current pane.
data Panel n appev state (panes :: [Type]) where
  Panel :: state -> Panel n appev state '[]
  PanelWith :: ( Pane n appev pane u
               , DrawConstraints pane (Panel n appev state panes) n
               , EventConstraints pane (Panel n appev state panes)
               , DispatchEvent n appev pane (EventType pane n appev)
               )
            => PaneState pane appev -> PaneFocus n
            -> Panel n appev state panes -> Panel n appev state (pane ': panes)


-- | This is the base constructor for Panel that is given the core
-- application state.
basePanel :: state -> Panel n appev state '[]
basePanel = Panel


-- | Each 'Pane' that is part of the 'Panel' should be added to the 'Panel' via
-- this function, which also specifies when the `Pane` should receive Events.
addToPanel :: Pane n appev pane u
           => InitConstraints pane (Panel n appev state panes)
           => DrawConstraints pane (Panel n appev state panes) n
           => EventConstraints pane (Panel n appev state panes)
           => DispatchEvent n appev pane (EventType pane n appev)
           => PaneFocus n
           -> Panel n appev state panes
           -> Panel n appev state (pane ': panes)
addToPanel n pnl = PanelWith (initPaneState pnl) n pnl


-- | Specifies when a Pane should receive events.
data PaneFocus n =
  -- | Indicates that this Pane always receives all events, although it is never
  --   part of a focus ring.  This should be used for Widgets that have a global
  --   event handling.
  Always
  -- | Indicates that this Pane's handlePaneEvent is never called
  | Never
  -- | Indicates that the pane should receive events when the current focus is
  --   equal to a 'focusable' return from the Pane.
  | WhenFocused
  -- | Indicates that the pane should receive events when the current focus is
  --   equal to a 'focusable' return from the Pane, and that this should block
  --   all non-modal focus candidates (it is expected that there is only one
  --   modal, but this is not required).
  | WhenFocusedModal
  | WhenFocusedModal' (FocusRing n)  -- previous focus ring to return to
  -- | Indicates that the pane should receive events when the current focus is
  -- equal to a 'focusable' return from the Pane, and that this should block all
  -- non-modal focus candidates, just as with 'WhenFocusedModal'.  However, this
  -- also sends *all* events to the modal Pane instead of the normal 'Panel'
  -- handling of events (e.g.  @TAB@/@Shift-TAB@).
  | WhenFocusedModalHandlingAllEvents
  | WhenFocusedModalHandlingAllEvents' (FocusRing n)  -- previous focus ring


-- | If the base state provides Focus information, then the Panel can provide
-- focus information.
instance HasFocus appState n => HasFocus (Panel n appEv appState panes) n where
  getFocus = onBaseState . getFocus


-- | This is a lens providing access to the base application state at
-- the core of the Panel.
onBaseState :: Lens' (Panel n appev state panes) state
onBaseState f (Panel s) = Panel <$> f s
onBaseState f (PanelWith p n i) = PanelWith p n <$> onBaseState f i

-- | This is a lens providing access to the PaneState for a specific Pane in the
-- Panel.  The Pane is typically specified via a type application
-- (e.g. @@MyPane@).
onPane :: forall pane n appev state panes .
          PanelOps pane n appev panes state
       => Lens' (Panel n appev state panes) (PaneState pane appev)
onPane = lens (panelState @pane) (panelStateUpdate @pane)


-- -- | This can be used to get the inner Pane from the current Pane in the state.
-- onNextPane :: Lens' (Panel n appev state (pane ': panes)) (Panel n appev state panes)
-- onNextPane f = \case
--   PanelWith a b r -> (\r' -> PanelWith a b r') <$> f r


-- | This class defines the various operations that can be performed
-- on a Panel.  Most of these operations specify a particular Pane as
-- the target of the operation; the operation is performed on that
-- pane and the Panel is is updated with the result.
--
-- The user of this library will not need to develop new instances of this class:
-- the instances defined internally are sufficient.  Users may need to specify
-- 'PanelOps' constraints on various functions.
class PanelOps pane n appev panes s | pane -> n where

  -- | This is called to pass the VTY Event to the specified Pane's
  -- handler with a Panel.
  handlePanelEvent :: (EventConstraints pane s, Eq n)
                   => s -> pane -> Panel n appev s panes -> BrickEvent n appev
                   -> EventM n es (Panel n appev s panes)

  -- | This is used to obtain the state of a specific Pane within the Panel.  The
  -- pane is usually specified by a type application (e.g. @@MyPane@).
  panelState :: Panel n appev s panes -> PaneState pane appev

  -- | This is used to update the state of a specific Pane within the Panel. The
  -- pane is usually specified by a type application (e.g. @@MyPane@).
  panelStateUpdate :: Panel n appev s panes -> PaneState pane appev
                   -> Panel n appev s panes

  -- | This returns an ordinal index of the pane within the panel.
  paneNumber :: Panel n appev s panes -> PaneNumber


instance (Pane n appev pane u) => PanelOps pane n appev (pane ': panes) s where
  handlePanelEvent s _p (PanelWith pd n r) ev =
    (\pd' -> PanelWith pd' n r) <$> dispEv Refl s ev pd
  panelState (PanelWith pd _ _) = pd
  panelStateUpdate (PanelWith _pd n r) = \pd' -> PanelWith pd' n r
  paneNumber _ = PaneNo 0


instance {-# OVERLAPPABLE #-} (PanelOps pane n appev panes s) =>
  PanelOps pane n appev (o ': panes) s where
  handlePanelEvent s p (PanelWith pd n r) ev =
    PanelWith pd n <$> handlePanelEvent s p r ev
  panelState (PanelWith _ _ r) = panelState r
  panelStateUpdate (PanelWith pd n r) =
    \pd' -> PanelWith pd n $ panelStateUpdate r pd'
  paneNumber (PanelWith _ _ r) = succ $ paneNumber @pane r


instance ( TypeError
           ('Text "No " ':<>: 'ShowType pane ':<>: 'Text " in Panel"
            ':$$: 'Text "Add this pane to your Panel (or move it lower)"
            ':$$: 'Text "(Possibly driven by DrawConstraints)"
           )
         , Pane n appev pane u
         )
  => PanelOps pane n appev '[] s where
  handlePanelEvent = absurd (undefined :: Void)
  panelState = absurd (undefined :: Void)
  panelStateUpdate = absurd (undefined :: Void)
  paneNumber = absurd (undefined :: Void)


-- | Called to draw a specific pane in the panel.  Typically invoked from the
-- applications' global drawing function.
panelDraw :: forall pane n appev s panes u .
             ( DrawConstraints pane (Panel n appev s panes) n
             , PanelOps pane n appev panes s
             , Pane n appev pane u
             , Eq n
             )
          => Panel n appev s panes -> Maybe (Widget n)
panelDraw panel = drawPane (panelState @pane panel) panel


-- | Called to dispatch an events to the focused Pane in the Panel as determined
-- by matching the Widget names returned by the Pane's 'focusable' with the
-- current FocusRing focus target.
handlePanelEvents :: Eq n
                  => Panel n appev s panes
                  -> BrickEvent n appev
                  -> Focused n
                  -> EventM n es (Panel n appev s panes)
handlePanelEvents panel ev (Focused focus) =
  -- n.b. no need to check focusable for a pane because an invisible
  -- pane should never have focus
  case focus of
    Nothing -> return panel
    Just fcs -> go fcs panel ev
  where
    go :: Eq n
       => n -> Panel n appev s panes -> BrickEvent n appev
       -> EventM n es (Panel n appev s panes)
    go _ p@(Panel {}) _ = return p
    go fcs (PanelWith pd pf r) evnt =
      let handleIt = dispEv Refl r evnt pd
          skipIt = return pd
      in do pd' <- case pf of
                     Never -> skipIt
                     Always -> handleIt
                     WhenFocused -> if fcs `elem` focusable r pd
                                    then handleIt
                                    else skipIt
                     WhenFocusedModal -> if fcs `elem` focusable r pd
                                         then handleIt
                                         else skipIt
                     WhenFocusedModal' _ -> if fcs `elem` focusable r pd
                                            then handleIt
                                            else skipIt
                     WhenFocusedModalHandlingAllEvents ->
                       if fcs `elem` focusable r pd
                       then handleIt
                       else skipIt
                     WhenFocusedModalHandlingAllEvents' _ ->
                       if fcs `elem` focusable r pd
                       then handleIt
                       else skipIt
            PanelWith pd' pf <$> go fcs r evnt


-- | Called to handle events for the entire 'Panel', including focus-changing
-- events.  The current focused 'Pane' is determined and that Pane's handler is
-- called (based on the 'Widget' names returned as 'focusable' for that Pane).
-- If a Pane has no associated Widget name (the 'PaneFocus' value is specified as
-- 'Nothing' when adding the Pane to the Panel) then its handler is never called.
--
-- This function returns the updated Panel state, as well as an indication of
-- whether a modal transition occured while handling the event.
--
-- This function manages updating the focus when @Tab@ or @Shift-Tab@ is
-- selected, except when the currently focused pane was created with the
-- 'WhenFocusedModalHandlingAllEvents', in which case all events are passed
-- through to the Pane.
handleFocusAndPanelEvents :: Eq n => Ord n
                          => Lens' (Panel n appev s panes) (FocusRing n)
                          -> Panel n appev s panes
                          -> BrickEvent n appev
                          -> EventM n es (PanelTransition, Panel n appev s panes)
handleFocusAndPanelEvents focusL panel =
  let fcs = focusGetCurrent (panel ^. focusL)
      doPanelEvHandling = case fcs of
                            Nothing -> True
                            Just curFcs -> chkEv curFcs panel
  in \case
    VtyEvent (Vty.EvKey (Vty.KChar '\t') []) | doPanelEvHandling ->
      return (Nothing, panel & focusL %~ focusNext)
    VtyEvent (Vty.EvKey Vty.KBackTab []) | doPanelEvHandling ->
      return (Nothing, panel & focusL %~ focusPrev)
    panelEv -> do
      u <- focusRingUpdate focusL <$> handlePanelEvents panel panelEv (Focused fcs)
      let fcs' = focusGetCurrent (u ^. focusL)
      if fcs == fcs'
        then return (Nothing, u)
        else let m0 = modalTgt (L.sort $ focusRingToList (panel ^. focusL)) panel
                 m1 = modalTgt (L.sort $ focusRingToList (u ^. focusL)) u
                 -- Note that the focusL-retrieved focus rings (m0, at least)
                 -- come from the live previous pane set and may not match the
                 -- order of the widget set.  Oddly, it doesn't match a rotation
                 -- of the original either, ergo the sorting.
             in return $ if m0 == m1
                         then (Nothing, u)
                         else (Just (m0, m1), u)
  where
    chkEv :: Eq n => n -> Panel n appev s panes -> Bool
    chkEv curFcs = \case
      Panel {} -> True
      PanelWith pd WhenFocusedModalHandlingAllEvents r ->
        (not $ curFcs `elem` focusable r pd) && chkEv curFcs r
      PanelWith pd (WhenFocusedModalHandlingAllEvents' _) r ->
        (not $ curFcs `elem` focusable r pd) && chkEv curFcs r
      PanelWith _ _ r -> chkEv curFcs r
    modalTgt :: Eq n => Ord n
             => [n] -> Panel n appev s panes -> PanelMode
    modalTgt fcsRing = \case
      Panel {} -> Normal
      PanelWith pd WhenFocusedModal r ->
        matchOrRecurse fcsRing r $ focusable r pd
      PanelWith pd (WhenFocusedModal' _) r ->
        matchOrRecurse fcsRing r $ focusable r pd
      PanelWith pd WhenFocusedModalHandlingAllEvents r ->
        matchOrRecurse fcsRing r $ focusable r pd
      PanelWith pd (WhenFocusedModalHandlingAllEvents' _) r ->
        matchOrRecurse fcsRing r $ focusable r pd
      PanelWith _ _ r -> case modalTgt fcsRing r of
                           Normal -> Normal
                           Modal p -> Modal $ succ p
    matchOrRecurse :: Eq n => Ord n
                   => [n] -> Panel n appev s pnlpanes -> Seq.Seq n -> PanelMode
    matchOrRecurse fcsRing r f =
      -- if fcsRing `elem` rotations (F.toList f)
      if fcsRing == L.sort (F.toList f)
      then Modal (PaneNo 0)
      else case modalTgt fcsRing r of
             Normal -> Normal
             Modal p -> Modal $ succ p



-- | Indicates the current mode of the Panel
data PanelMode = Normal | Modal PaneNumber deriving (Eq)

-- | Internal bookkeeping to identify a particular Pane within a Panel by number.
newtype PaneNumber = PaneNo Natural deriving (Eq, Enum)


-- | This is returned from the 'handleFocusAndPanelEvents' function to indicate
-- whether a modal transition occured during the panel's (and associated Pane's)
-- handling of this event.  This can be used by the outer-level application code
-- to determine if a modal Pane was entered or exited due to the Event.
type PanelTransition = Maybe (PanelMode, PanelMode)


-- | Indicates if the specified Pane (via Type Application) is the one that was
-- modally entered as a result of processing an event (as indicated by
-- PanelTransition).
enteredModal :: forall pane n appev state panes
               . PanelOps pane n appev panes state
               => PanelTransition -> Panel n appev state panes -> Bool
               -- n.b. assumes the Panel passed here is the same panel passed to
               -- handleFocusAndPanelEvents for which the PanelTransition was
               -- obtained
enteredModal = \case
  Just (_, Modal pnum) -> (pnum ==) . paneNumber @pane
  _ -> const False


-- | Indicates if the specified Pane (via Type Application) is the one that was
-- modally exited (dismissed) as a result of processing an event (as indicated by
-- PanelTransition).
exitedModal :: forall pane n appev state panes
               . PanelOps pane n appev panes state
               => PanelTransition -> Panel n appev state panes -> Bool
               -- n.b. assumes the Panel passed here is the same panel passed to
               -- handleFocusAndPanelEvents for which the PanelTransition was
               -- obtained
exitedModal = \case
  Just (Modal pnum, _) -> (pnum ==) . paneNumber @pane
  _ -> const False


-- | When the Panel is managing focus events (e.g. when using
-- 'handleFocusAndPanelEvents'), this function can be called if there
-- has been a situation where the members of the focus ring might need
-- to be updated.  This is automatically called at the end of the
-- 'handleFocusAndPanelEvents', but it should be explicitly called
-- once when the Panel is initialized, and it can additionally be
-- called whenever needed in a situation where the
-- 'handleFocusAndPanelEvents' invocation is insufficient (e.g. a
-- separate global action enables a modal pane).
focusRingUpdate :: (Eq n, Ord n)
                => Lens' (Panel n appev s panes) (FocusRing n)
                -> Panel n appev s panes -> Panel n appev s panes
focusRingUpdate focusL panel = let (p', r) = focusableNames focusL panel
                               in p' & focusL %~ updRing r
  where
    updRing :: Eq n => [n] -> FocusRing n -> FocusRing n
    updRing nl fcs =
      case nl of
        [] -> focusRing []
        (n : _) ->
          case focusGetCurrent fcs of
            Nothing ->
              -- no current focus, just use new list
              focusSetCurrent n $ focusRing nl
            Just e ->
              case L.find ((e ==) . head) $ rotations nl of
                Just r ->
                  focusRing r -- new ring with current element still focused
                Nothing ->
                  -- new focus ring doesn't include current focused
                  -- element, so just use the new list.
                  focusSetCurrent n $ focusRing nl


-- | This returns the focusable Widget names for the focus ring, in the 'Ord'
-- order.  It also returns an updated panel, which internally records the input
-- focus ring if a modal is selected).  If the previous focus was a modal and the
-- new focus is not modal, this will return that previous focus ring rather than
-- the computed focus ring.
focusableNames :: (Eq n, Ord n)
               => Lens' (Panel n appev s panes) (FocusRing n)
               -> Panel n appev s panes -> (Panel n appev s panes, [n])
focusableNames focusL panel = finish $ subFocusable focusL panel panel
  where
    finish ((prvFcs, pnl), (mdlFcs, regFcs)) =
      let reorder = F.toList . Seq.sort
          fr = if null mdlFcs
               then fromMaybe (reorder regFcs) prvFcs
               else reorder mdlFcs
      in (pnl, fr)

subFocusable :: Eq n
             => Lens' (Panel n appev s panes) (FocusRing n)
             -> Panel n appev s panes -> Panel n appev s rempanes
             -> ((Maybe [n], Panel n appev s rempanes), (Seq n, Seq n))
subFocusable focusL base = \case
  i@(Panel {}) -> ((Nothing, i), (mempty, mempty))
  PanelWith pd WhenFocused r ->
    let (i', ns) = subFocusable focusL base r
        ns' = let pf = focusable r pd
              in (fst ns, pf >< snd ns)
    in (PanelWith pd WhenFocused <$> i', ns')
  PanelWith pd WhenFocusedModal r ->
    let (f', pf', i', ns') = goModal focusL base pd Nothing r
        pfNew = case pf' of
                  Nothing -> WhenFocusedModal
                  Just x -> WhenFocusedModal' x
    in ((f', PanelWith pd pfNew i'), ns')
  PanelWith pd (WhenFocusedModal' pf) r ->
    let (f', pf', i', ns') = goModal focusL base pd (Just pf) r
        pfNew = case pf' of
                  Nothing -> WhenFocusedModal
                  Just x -> WhenFocusedModal' x
    in ((f', PanelWith pd pfNew i'), ns')
  PanelWith pd WhenFocusedModalHandlingAllEvents r ->
    let (f', pf', i', ns') = goModal focusL base pd Nothing r
        pfNew = case pf' of
                  Nothing -> WhenFocusedModalHandlingAllEvents
                  Just x -> WhenFocusedModalHandlingAllEvents' x
    in ((f', PanelWith pd pfNew i'), ns')
  PanelWith pd (WhenFocusedModalHandlingAllEvents' pf) r ->
    let (f', pf', i', ns') = goModal focusL base pd (Just pf) r
        pfNew = case pf' of
                  Nothing -> WhenFocusedModalHandlingAllEvents
                  Just x -> WhenFocusedModalHandlingAllEvents' x
    in ((f', PanelWith pd pfNew i'), ns')
  PanelWith x y r -> let (i', ns) = subFocusable focusL base r
                     in (PanelWith x y <$> i', ns)

goModal :: fullpanel ~ Panel n appev s panes
        => rempanel ~ Panel n appev s rempanes
        => EventConstraints pane rempanel
        => Pane n appev pane updateType
        => Eq n
        => Lens' fullpanel (FocusRing n)
        -> fullpanel
        -> PaneState pane appev
        -> Maybe (FocusRing n)
        -> rempanel
        -> (Maybe [n], Maybe (FocusRing n), rempanel, (Seq n, Seq n))
goModal focusL base pd pf r =
      let ((f, i'), ns) = subFocusable focusL base r
          fnms = focusable r pd
          fpred = not $ Seq.null fnms
          ns' =  (fnms >< fst ns, snd ns)
          f' = if fpred then Nothing else f <|> (focusRingToList <$> pf)
          pf' = if fpred then pf <|> Just (base^.focusL) else Nothing
      in (f', pf', i', ns')



-- | This returns all shrl instances of the input list.
--
--  rotations [1,2,3] == [ [1,2,3], [2,3,1], [3,1,2] ]
--  rotations [1,2,3,4] == [ [1,2,3,4], [2,3,4,1], [3,4,1,2], [4,1,2,3] ]
--  rotations [1] == [ [1] ]
--  rotations [] == []
rotations :: [a] -> [ [a] ]
rotations l = map rotateBy $ [0..length l - 1]
  where rotateBy n = uncurry (flip (<>)) $ L.splitAt n l
