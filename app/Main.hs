module Main where

import           Control.Monad
import qualified Graphics.UI.Threepenny                 as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Ext.Contextmenu

-- | Runs the example.
main :: IO ()
main = startGUI defaultConfig example

-- | Example use of threepenny contextmenu.
example :: Window -> UI ()
example window = void $ do
    body <- getBody window
    button <- UI.button # set UI.text "right-click me!"
    contextMenu (menuItems button) body
    element body #+ [element button]

-- | Menu items to change an element red or blue.
menuItems :: Element -> [MenuItem Element]
menuItems el = [
        actionMenuItem "red"   [colour el "red" ],
        actionMenuItem "blue"  [colour el "blue"],
        nestedMenuItem "more" [
                actionMenuItem "green" [colour el "green"],
                nestedMenuItem "even more" [
                    actionMenuItem "red"   [colour el "red" ],
                    actionMenuItem "blue"  [colour el "blue"]
                ],
                nestedMenuItem "even more" [
                    actionMenuItem "red"   [colour el "red" ],
                    actionMenuItem "blue"  [colour el "blue"]
                ]
            ]
    ]

-- | Sets an element's colour to the given string.
colour :: Element -> String -> UI Element
colour el str = element el # set style [("color", str)]

