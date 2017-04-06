module Graphics.UI.Threepenny.Ext.Contextmenu.Util where

import           Control.Monad               (void)
import           Data.Maybe                  (catMaybes)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

-- |Set "display: X;" on an element.
display :: String -> Element -> UI Element
display x el = element el # set style [("display", x)]

-- |Display an element at given coordinates.
displayAt :: Int -> Int -> Element -> UI Element
displayAt x y el = do
  element el # set style [("left", show x ++ "px"), ("top", show y ++ "px")]
  display "block" el

-- |Set CSS dimensions of an element to the given values.
dimensions :: String -> String -> Element -> UI Element
dimensions w h el = element el # set style [("width", w), ("height", h)]

-- |A little bit of gymastics to restructure the given data.
extract :: [(Element, Maybe [UI ()])] -> UI ([Element], [UI ()])
extract tuples = return (map fst tuples, concat $ catMaybes $ map snd tuples)

-- |Execute one UI action on hover and another on leave.
whileHover :: Element -> UI a -> UI b -> UI ()
whileHover el onHover onLeave = void $ do
  on UI.hover el $ const onHover
  on UI.leave el $ const onLeave

-- |For each element in the list apply a user defined function which takes the
-- current element and its index (a, Int), and the original list with indexed
-- elements [(a, Int)].
mapPairsWithIndex :: [a] -> ((a, Int) -> [(a, Int)] -> UI ()) -> UI ()
mapPairsWithIndex xs f = do
  let indexedXs = zip xs [1..]
  mapM_ (flip f indexedXs) indexedXs
