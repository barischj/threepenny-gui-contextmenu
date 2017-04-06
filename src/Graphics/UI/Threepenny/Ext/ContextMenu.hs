-- This module could potentially be rewritten and heavily simplified once the
-- custom context menu spec in HTML 5.1 is adopted by all major browsers. At
-- time of writing the spec is only a recommendation and only implemented by
-- Mozilla Firefox.

module Graphics.UI.Threepenny.Ext.Contextmenu (
  -- * Menu items and constructors.
  MenuItem(..), MenuItemValue(..), actionMenuItem, nestedMenuItem,
  -- * Context menu.
  contextMenu
  ) where

import           Control.Monad                                (void, when)
import qualified Graphics.UI.Threepenny                       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Ext.Contextmenu.Style
import           Graphics.UI.Threepenny.Ext.Contextmenu.Util


-- |A menu item has some text to display and a 'MenuItemValue'.
data MenuItem = MenuItem { mIText :: String, mIValue :: MenuItemValue }

-- |UI actions to execute or a nested menu.
data MenuItemValue = MenuItemActions [UI ()] | NestedMenu [MenuItem]

-- |Constructor for a menu item with UI actions to execute.
actionMenuItem :: String -> [UI u] -> MenuItem
actionMenuItem text actions =
  MenuItem { mIText = text, mIValue = MenuItemActions $ map void actions }

-- |Constructor for a menu item with a nested menu.
nestedMenuItem :: String -> [MenuItem] -> MenuItem
nestedMenuItem text nested =
  MenuItem { mIText = text ++ "  ›", mIValue = NestedMenu nested }

-- |Attach a context menu to the document body. The context menu is activated by
-- a contextmenu event from the given element. The event is prevented from
-- propagating.
contextMenu :: [MenuItem] -> Element -> UI ()
contextMenu items source = do
  rmTarget <- UI.div # set style rmTargetStyle
  let closeRmTarget = void $ dimensions "0" "0" rmTarget
  (menu, closeMenu, closeMenusNestedMenus) <- newMenu [closeRmTarget] items
  -- Display menu on a contextmenu event.
  on UI.contextmenu source $ \(x, y) ->
    displayAt x y menu >> dimensions "100vw" "100vh" rmTarget
  -- Hide everything on rmTarget click.
  on UI.mousedown rmTarget $ const $
     closeRmTarget >> closeMenu >> sequence closeMenusNestedMenus
  -- Hide nested menus on hover over rmTarget.
  on UI.hover rmTarget $ const $ sequence closeMenusNestedMenus
  -- Attach everything to the body, with a large z-index.
  parent <- UI.div #+ [element rmTarget, element menu]
                   # set UI.style [("z-index", "10000"),
                                   ("position", "absolute")]
  (askWindow >>= getBody) #+ [element parent]
  preventDefaultContextMenu source

-- |A tuple of a menu element, UI action to close it, and UI actions to close
-- any nested menus.
newMenu :: [UI ()] -> [MenuItem] -> UI (Element, UI (), [UI ()])
newMenu closeParents menuItems = do
  menu <- UI.li # set style menuStyle
  let closeMenu = void $ display "none" menu
  -- Tuples of menu items and UI actions to close respective nested menus.
  -- :: UI [(Element, [Action])]
  menuItemEls <- mapM (menuItem $ closeParents ++ [closeMenu]) menuItems
  element menu #+ map (element . fst) menuItemEls
  -- On hover over a menu item close any nested menus from *other* menu items.
  let closeOtherMenusOnHover ((el1, _), i1) xs =
        on UI.hover el1 $ const $ do
          let closeIfNotSelf ((_, closeEl2), i2) =
                when (i1 /= i2) (sequence_ closeEl2)
          mapM closeIfNotSelf xs
  mapPairsWithIndex menuItemEls closeOtherMenusOnHover
  return (menu, closeMenu, concat $ map snd menuItemEls)

-- |A tuple of a menu item element, and UI actions to close it and a potential
-- nested menu.
menuItem :: [UI ()] -> MenuItem -> UI (Element, [UI ()])
menuItem closeParents (MenuItem text value) = do
  menuItem <- UI.li # set UI.text text # set style menuItemStyle
  highlightWhileHover menuItem
  case value of
    MenuItemActions actions -> do
      -- On click close the entire menu and execute the UI actions.
      on UI.click menuItem $ const $ sequence $ closeParents ++ actions
      return (menuItem, [])
    NestedMenu nestedMenuItems -> do
      (nestedMenu, closeNestedMenu, closeNestedMenusNestedMenus)
         <- newMenu closeParents nestedMenuItems
      -- Position a nested menu relative to this menu item.
      -- element menuItemEl # set UI.position "relative"
      -- element nestedMenuEl # set UI.position "absolute" # set UI.right "0px" # set UI.top "0px"
      element menuItem #+ [element nestedMenu]
      -- On hover display the nested menu.
      on UI.hover menuItem $ const $ display "block" nestedMenu
      return (menuItem, [closeNestedMenu] ++ closeNestedMenusNestedMenus)

-- |Highlight an element while hovered over.
highlightWhileHover :: Element -> UI ()
highlightWhileHover el = whileHover el
  (element el # set style [("background-color", "#DEF"   )])
  (element el # set style [("background-color", "inherit")])

-- |CSS class used to identify elements on which to prevent a default context
-- menu from opening.
preventDefaultClass = "__prevent-default-context-menu"

-- |Prevent a default context menu opening on an element.
preventDefaultContextMenu :: Element -> UI ()
preventDefaultContextMenu el = do
  element el # set UI.class_ preventDefaultClass
  runFunction $ ffi "$(%1).bind('contextmenu', e => e.preventDefault())"
                    ("." ++ preventDefaultClass)
