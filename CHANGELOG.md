# Revision history for brick-panes

## 1.0.0.4 -- 2023-03-30

* Bump upper bounds to allow brick 1.6 and vty 5.39 and text-zipper 0.13.

## 1.0.0.3 -- 2022-12-18

* Bump upper bound for brick to allow brick 1.5.
* Updated haddocks to describe default methods for `focusable` and
  `handlePaneEvent` and enhance other descriptions.
* Use `putCursor` in example application to indicate focus (helps screen
  readers).  Thanks due to Mario Lang.

## 1.0.0.2 -- 2022-11-01

* Bump upper bound for brick to allow brick 1.4.

## 1.0.0.1 -- 2022-10-13

* Documentation improvements.

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
