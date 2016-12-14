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
    element body #+ [element button]
    let menuItems = [
                ("red",  [colour button "red" ]),
                ("blue", [colour button "blue"])
            ]
    Menu.contextMenu menuItems body

colour :: Element -> String -> UI Element
colour el string = element el # set style [("color","#CCAABB")]
