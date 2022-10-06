# Revision history for brick-panes

## 1.0.0.0 -- 2022-10-01

* Public release.

## 0.3.0.0 -- 2022-09-29

* Updated `handleFocusAndPanelEvents` to return a `PanelTransition` indication
  along with the new `Panel`.  This can be used by applications perform
  additional actions if the event just handled caused a transition into or out-of
  a modal Pane.
* The `exitedModal` and `enteredModal` functions can be used to determine the
  type of transition (if any).
* The `isPanelModal` function can be used to determine the current modal state of
  the Panel.

## 0.2.0.0 -- 2022-09-22

* Removed argument from `WhenFocusedModal` and
  `WhenFocusedModalHandlingAllEvents` `PaneFocus` constructors.  These arguments
  should always have been specified as `Nothing` by client code, with deleterious
  behavior if this requirement was not followed, so the need for this awkward
  constant representation was removed.

## 0.1.0.0 -- 2022-09-10

* Initial version.
