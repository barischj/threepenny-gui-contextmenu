module Lib (run) where

import Control.Monad
import qualified ContextMenu as Menu
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

-- |TODO Show client side debug messages
-- |TODO Create binding between threepenny-gui and Polymer
-- |TODO Return entire event to Haskell

run :: IO ()
run = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = void $ do
    body <- getBody window
    button <- UI.button # set UI.text "change my colour via right-click"
    Menu.contextMenu (exampleMenu button) body
    element body #+ [element button]

-- |Sets an element's colour to the given string.
colour :: Element -> String -> UI Element
colour el string = element el # set style [("color", string)]

-- |An example menu that changes an element red or blue.
exampleMenu :: Element -> [Menu.MenuItem Element]
exampleMenu el = [
    Menu.actionMenuItem "red"   [colour el "red" ],
    Menu.actionMenuItem "blue"  [colour el "blue"]
  ]
