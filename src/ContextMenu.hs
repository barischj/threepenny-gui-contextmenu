module ContextMenu where

import           Control.Monad               (msum, when)
import           Data.Maybe                  (catMaybes)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

-- | Default style for the context menu.
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

-- | Default style for any menu items.
menuItemStyle = [
        ("cursor",  "pointer"),
        ("padding", "8px 12px")
    ]

-- | Full-screen transparent target to close the menu.
rmTargetStyle = [
        ("height",   "0"),
        ("left",     "0"),
        ("position", "absolute"),
        ("top",      "0"),
        ("width",    "0")
    ]

type Action = UI Element

-- | A menu item is some text to be displayed and either UI actions to execute
--   or a nested menu.
data MenuItem a = MenuItem { mIText :: String, mIValue :: MenuItemValue a }
data MenuItemValue a = MenuItemActions [UI a] | NestedMenu [MenuItem a]

-- | Constructor for a menu item that contains UI actions to execute.
actionMenuItem :: String -> [UI a] -> MenuItem a
actionMenuItem text actions =
    MenuItem { mIText = text, mIValue = MenuItemActions actions }

-- | Constructor for a menu item that contains a nested menu.
nestedMenuItem :: String -> [MenuItem a] -> MenuItem a
nestedMenuItem text nested =
    MenuItem { mIText = text ++ "  ›", mIValue = NestedMenu nested }

-- | Attaches a custom context menu to an element.
contextMenu :: [MenuItem a] -> Element -> UI ()
contextMenu items source = do
    rmTarget      <- UI.div # set style rmTargetStyle
    let closeRMTarget =
          element rmTarget # set style [("width", "0"), ("height", "0")]
    (menu, close) <- newMenu closeRMTarget items
    -- Define functions to open and close the menu.
    let openMenu (x, y) = do
          element menu # set style
            [("left", show x ++ "px"), ("top", show y ++ "px"),
             ("display", "block")]
          element rmTarget # set style
            [("width", "100vw"), ("height", "100vh")]
        closeMenu = do
          element menu     # set style [("display", "none")]
    -- Add menu items to the menu.
    -- element menu #+ map (menuItem closeMenu) items
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

-- | Returns a menu element and a function to close it.
newMenu :: Action -> [MenuItem a] -> UI (Element, [Action])
newMenu closeAbove menuItems = do
    -- Create a blank menu and function to close it.
    menuEl <- UI.li # set style menuStyle
    let closeMenu = element menuEl # set style [("display", "none")]
    -- Menu items as elements and respective list of actions to close nested
    -- menus. :: UI [(Element, [Action])]
    tuples <- mapM (menuItem closeMenu) menuItems 
    -- Append all menu items to the menu.
    element menuEl #+ map (element . fst) tuples
    -- :: [((Element, [Action]), Integer)]
    let indexedCloseNested = zip tuples [1..]
        -- On hover over a menu item we want close any nested menus from
        -- *other* menu items. To do this we map the following function over
        -- all menu items. This function takes an element from the list and
        -- when it is hovered over will run the close actions of all *other*
        -- menu items.
        closeOtherMenusOnHover ((el1, _), index1) =
            on UI.hover el1 $ const $ do
                let closeIfNotSelf ((_, actions2), index2) =
                      when (index1 /= index2) (sequence_ actions2)
                mapM closeIfNotSelf indexedCloseNested
    mapM closeOtherMenusOnHover indexedCloseNested
    return (menuEl, [closeMenu] ++ concat (map snd tuples))

-- | Returns a menu item element and actions to close a nested menu.
menuItem :: Action -> MenuItem a -> UI (Element, [Action])
menuItem closeRoot (MenuItem text value) = do
    menuItemEl <- UI.li # set UI.text text # set style menuItemStyle
    highlightWhileHover menuItemEl
    case value of
        MenuItemActions actions -> do
            -- On click execute the actions and close the entire menu.
            on UI.click menuItemEl $ const $ do
                closeRoot
                liftIO $ putStrLn "event clicked"
                sequence_ actions
            return (menuItemEl, [])
        NestedMenu nestedMenuItems -> do
            (nestedMenuEl, closeNested) <- newMenu closeRoot nestedMenuItems
            element menuItemEl #+ [element nestedMenuEl]
            -- On hover display the nested menu.
            on UI.hover menuItemEl $ const $
                element nestedMenuEl # set style [("display", "block")]
            return (menuItemEl, closeNested)

-- | A little bit of gymastics to restructure the given data.
extract :: [(Element, Maybe [Action])] -> UI ([Element], [Action])
extract tuples = return (map fst tuples, concat $ catMaybes $ map snd tuples)

-- | Highlights an element while being hovered over.
highlightWhileHover :: Element -> UI ()
highlightWhileHover el = whileHover el
    (element el # set style [("background-color", "#DEF"   )])
    (element el # set style [("background-color", "inherit")])

-- | Execute one action on hover and another on leave.
whileHover :: Element -> Action -> Action -> UI ()
whileHover el onHover onLeave = do
    on UI.hover el $ const onHover
    on UI.leave el $ const onLeave

-- | CSS class used to identify elements on which to prevent a default context
--   menu from opening.
preventDefaultClass = "__prevent-default-context-menu"

-- | Prevents a default context menu opening from the given element.
preventDefaultContextMenu :: Element -> UI ()
preventDefaultContextMenu el = do
    element el # set UI.class_ preventDefaultClass
    runFunction $ ffi "$(%1).bind('contextmenu', e => e.preventDefault())"
                      ("." ++ preventDefaultClass)
