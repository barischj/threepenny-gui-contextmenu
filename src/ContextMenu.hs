module ContextMenu (
    contextMenu
    ) where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

menuStyle = [
        ("background",      "#FFF"),
        ("border",          "1px solid #CCC"),
        ("border-radius",   "3px"),
        ("color",           "#333"),
        ("display",         "none"),
        ("padding-left",    "0"),
        ("list-style-type", "none"),
        ("position",        "absolute")
    ]

menuItemStyle = [
        ("cursor",  "pointer"),
        ("padding", "8px 12px")
    ]

rmTargetStyle = [
        ("position", "absolute"),
        ("height",   "100vh"),
        ("left",     "0"),
        ("top",      "0"),
        ("width",    "100vw")
    ]

-- |Attaches a custom context menu to an element.
contextMenu :: [String] -> Element -> UI Element
contextMenu items source = do
    rmTarget <- UI.div # set style rmTargetStyle
    menu' <- menu items
    element source #+ [UI.div #+ [element rmTarget, element menu']]
    -- Display the menu at mouse on a contextmenu event.
    on UI.contextmenu source $ \(x, y) ->
        element menu' # set style
            [("left", show x ++ "px"), ("top", show y ++ "px"),
             ("display", "block")]
    -- Hide the menu when the screen is clicked elsewhere.
    on UI.mousedown rmTarget $ const $
        element menu' # set style [("display", "none")]
    return source

-- |Returns a menu with given strings as menu items.
menu :: [String] -> UI Element
menu items = do
    menuElem <- UI.ul # set style menuStyle
    return menuElem #+ map menuItem items

-- |Returns a menu item from a string.
menuItem :: String -> UI Element
menuItem item = do
    menuItem <- UI.li # set UI.text item # set style menuItemStyle
    on UI.hover menuItem $ const $
        element menuItem # set style [("background-color", "#DEF")]
    on UI.leave menuItem $ const $
        element menuItem # set style [("background-color", "inherit")]
    return menuItem
