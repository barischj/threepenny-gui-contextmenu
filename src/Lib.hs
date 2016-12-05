module Lib
    ( run
    ) where

import Control.Monad
import qualified ContextMenu as Menu
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

-- |TODO Show custom context menu
-- 1. Capture contextmenu event DONE
-- 2. Cancel event propogation
-- 3. Show html menu at mouse
-- 4. Add handler to menu to destroy self
-- 5. Create function to add menu handler to element

-- |TODO Show client side debug messages

-- |TODO Create binding between threepenny-gui and Polymer

someFunc :: IO ()
someFunc = putStrLn "someFunc"

run :: IO ()
run = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    buttonA <- UI.button # set UI.text "Click me!"
    buttonB <- UI.button # set UI.text "Open context menu on me!"
    getBody window #+ [element buttonA, element buttonB]
    on UI.click buttonA $ \event ->
        element buttonA # set UI.text "I have been clicked!"
    on UI.contextmenu buttonB $ const $
        element buttonB # set UI.text "Context menu activated!"
    getBody window #+ [Menu.contextMenu ["foo", "bar", "car"]]
    return ()
