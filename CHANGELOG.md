# Revision history for brick-panes

## 0.2.0.0 -- 2022-09-22

* Removed argument from `WhenFocusedModal` and
  `WhenFocusedModalHandlingAllEvents` `PaneFocus` constructors.  These arguments
  should always have been specified as `Nothing` by client code, with deleterious
  behavior if this requirement was not followed, so the need for this awkward
  constant representation was removed.

## 0.1.0.0 -- 2022-09-10

* Initial version.
