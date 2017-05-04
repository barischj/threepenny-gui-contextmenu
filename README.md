# Threepenny-gui Context Menu

A right-click menu simply consists of menu items. These menu items can either
run UI actions when clicked, or contain a nested menu which is opened on hover.
We provide constructors for both of these types of menu items, those with UI
actions to run and those with a nested menu.

For the constructor of a menu item with UI actions you need to provide a title
and a list of the UI actions to run.

``` haskell
actionMenuItem "red" []
```

For the constructor of a menu item containing a nested menu item you need to
provide a title and a list of menu items.

``` haskell
ourMenuItems = [
    nestedMenuItem "more" [
      actionMenuItem "red"  []
    , actionMenuItem "blue" []
    ]
  ]
```

Finally to build a context menu we also need to know which element, when
clicked, activates the context menu.

``` haskell
someElement # contextMenu ourMenuItems
```
