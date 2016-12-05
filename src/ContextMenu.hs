module ContextMenu (
    contextMenu
    ) where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

menuStyle = [
        ("border",          "1px solid #CCC"),
        ("background",      "#FFF"),
        ("color",           "#333"),
        ("border-radius",   "3px"),
        ("position",        "absolute"),
        ("list-style-type", "none"),
        ("padding-left",    "0")
    ]

-- |Displays a custom context menu at the mouse on a contextmenu event.
contextMenu :: [String] -> UI Element
contextMenu strings = do
    menu' <- menu strings
    on UI.contextmenu menu' $ \(x, y) ->
        element menu' # set style
            [("left", show x ++ "px"), ("top", show y ++ "px")]
    return menu'

menu :: [String] -> UI Element
menu strings = do
    menuElem <- UI.ul # set style menuStyle
    return menuElem #+ map menuItem strings

menuItemStyle = [
        ("padding", "8px 12px"),
        ("cursor",  "pointer")
    ]

menuItem :: String -> UI Element
menuItem string = do
    menuItem <- UI.li # set UI.text string # set style menuItemStyle
    on UI.hover menuItem $ const $
        element menuItem # set style [("background-color", "#DEF")]
    on UI.leave menuItem $ const $
        element menuItem # set style [("background-color", "inherit")]
    return menuItem
