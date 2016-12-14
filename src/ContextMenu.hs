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
        ("position", "absolute"),
        ("height",   "100vh"),
        ("left",     "0"),
        ("top",      "0"),
        ("width",    "100vw")
    ]

-- |Attaches a custom context menu to an element.
contextMenu :: [(String, [UI b])] -> Element -> UI ()
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
    preventDefaultContextMenu source

-- |Returns a menu element with given strings as menu items.
menu :: [(String, [UI b])] -> UI Element
menu items = do
    menuEl <- UI.ul # set style menuStyle
    return menuEl #+ map menuItem items

-- |Returns a menu item element from a string.
menuItem :: (String, [UI b]) -> UI Element
menuItem (item, f) = do
    itemEl <- UI.li # set UI.text item # set style menuItemStyle
    on UI.hover itemEl $ const $
        element itemEl # set style [("background-color", "#DEF")]
    on UI.leave itemEl $ const $
        element itemEl # set style [("background-color", "inherit")]
    on UI.click itemEl $ const $ sequence_ f
    return itemEl

preventDefaultClass = "__prevent-default-context-menu"

-- |Prevents the default action on a contextmenu event.
preventDefaultContextMenu :: Element -> UI ()
preventDefaultContextMenu el = do
    element el # set UI.class_ preventDefaultClass
    runFunction $ ffi
        "$(%1).bind('contextmenu', e => e.preventDefault())"
        ("." ++ preventDefaultClass)
