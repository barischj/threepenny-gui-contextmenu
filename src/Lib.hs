module Lib (run) where

import qualified ContextMenu                 as Menu
import           Control.Monad
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

-- | Runs the example.
run :: IO ()
run = startGUI defaultConfig example

-- | Example use of threepenny contextmenu.
example :: Window -> UI ()
example window = void $ do
    body <- getBody window
    button <- UI.button # set UI.text "change my colour via right-click"
    Menu.contextMenu (menuItems button) body
    element body #+ [element button]

-- | Menu items to change an element red or blue.
menuItems :: Element -> [Menu.MenuItem Element]
menuItems el = [
        Menu.actionMenuItem "red"   [colour el "red" ],
        Menu.actionMenuItem "blue"  [colour el "blue"],
        Menu.nestedMenuItem "more"
            [Menu.actionMenuItem "green" [colour el "green"]]
    ]

-- | Sets an element's colour to the given string.
colour :: Element -> String -> UI Element
colour el str = element el # set style [("color", str)]

