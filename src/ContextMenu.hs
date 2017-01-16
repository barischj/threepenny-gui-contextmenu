module ContextMenu where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

menuStyle = [
        ("background",      "#FFF"),
        ("border",          "1px solid #CCC"),
        ("border-radius",   "3px"),
        ("color",           "#333"),
        ("display",         "none"),
        ("list-style-type", "none"),
        ("margin",          "0"),
        ("padding-left",    "0"),
        ("position",        "absolute")
    ]

menuItemStyle = [
        ("cursor",  "pointer"),
        ("padding", "8px 12px")
    ]

rmTargetStyle = [
        ("height",   "0"),
        ("left",     "0"),
        ("position", "absolute"),
        ("top",      "0"),
        ("width",    "0")
    ]

-- TODO
--   Nested menus 
--   2 space indentation

-- Summary of how the menu operates:
--
-- on source element right click:
--   set rmTarget size to 100vw x 100vh
--   set menu to display
-- on menu item or rmTarget click:
--   set rmTarget size to 0 x 0
--   set menu to display none
-- on menu item click:
--   run UI action

-- Menu items are either:
--   a string and [UI a]
--     - on click close menu and run UI actions
--   a string and [MenuItem]
--     - on hover:
--       - 

data MenuItem a = MenuItem { aText :: String, aValue :: MenuItemValue a }

data MenuItemValue a = MenuItemActions [UI a] | MoreMenuItems [MenuItem a]

actionMenuItem :: String -> [UI a] -> MenuItem a
actionMenuItem text actions =
  MenuItem { aText = text, aValue = MenuItemActions actions }

-- |Attaches a custom context menu to an element.
contextMenu :: [MenuItem a] -> Element -> UI ()
contextMenu items source = do
    rmTarget <- UI.div # set style rmTargetStyle
    menu <- UI.ul # set style menuStyle
    -- Define functions to open and close the menu.
    let openMenu (x, y) = do
          element menu # set style
            [("left", show x ++ "px"), ("top", show y ++ "px"),
             ("display", "block")]
          element rmTarget # set style
            [("width", "100vw"), ("height", "100vh")]
        closeMenu = do
          element menu     # set style [("display", "none")]
          element rmTarget # set style [("width", "0"), ("height", "0")]
    -- Add menu items to the menu.
    element menu #+ map (menuItem closeMenu) items
    -- Display menu on a contextmenu event.
    on UI.contextmenu source $ \(x, y) -> do
      liftIO $ putStrLn "context event fired"
      openMenu (x, y)
    -- Hide menu on rmTarget click.
    on UI.mousedown rmTarget $ const $ do
      closeMenu
      liftIO $ putStrLn "rmTarget clicked"
    -- Add elements to the given element.
    element source #+ [element rmTarget, element menu]
    -- Prevent the default context menu.
    preventDefaultContextMenu source

-- |Returns a menu and function to hide it.
newMenu :: [MenuItem a] -> UI (Element, UI Element)
newMenu items = do
  menuEl <- UI.li # set style menuStyle
  element menuEl #+ map (menuItem $ return ()) items
  let close = element menuEl # set style [("display", "none")]
  return (menuEl, close)

-- |Returns a menu item element and potentially a function to close a submenu.
menuItem :: UI a -> MenuItem b -> UI Element
menuItem close (MenuItem text value) = do
    itemEl <- UI.li # set UI.text text # set style menuItemStyle
    on UI.hover itemEl $ const $
        element itemEl # set style [("background-color", "#DEF")]
    on UI.leave itemEl $ const $
        element itemEl # set style [("background-color", "inherit")]
    case value of
      MenuItemActions f ->
        on UI.click itemEl $ const $ do
          close
          liftIO $ putStrLn "event clicked"
          sequence_ f
      MoreMenuItems lm -> do
        (subMenu, closeMenu) <- newMenu lm
        return ()
    return itemEl

preventDefaultClass = "__prevent-default-context-menu"

-- |Prevents the default action on a contextmenu event.
preventDefaultContextMenu :: Element -> UI ()
preventDefaultContextMenu el = do
    element el # set UI.class_ preventDefaultClass
    runFunction $ ffi
        "$(%1).bind('contextmenu', e => e.preventDefault())"
        ("." ++ preventDefaultClass)
