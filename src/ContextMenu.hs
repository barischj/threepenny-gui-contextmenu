module ContextMenu where

import           Control.Monad               (when)
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
data MenuItemValue a = MenuItemActions [UI Element] | NestedMenu [MenuItem a]

-- | Constructor for a menu item that contains UI actions to execute.
actionMenuItem :: String -> [UI Element] -> MenuItem a
actionMenuItem text actions =
    MenuItem { mIText = text, mIValue = MenuItemActions actions }

-- | Constructor for a menu item that contains a nested menu.
nestedMenuItem :: String -> [MenuItem a] -> MenuItem a
nestedMenuItem text nested =
    MenuItem { mIText = text ++ "  ›", mIValue = NestedMenu nested }

-- | Attaches a custom context menu to an element.
contextMenu :: [MenuItem a] -> Element -> UI ()
contextMenu items source = do
    rmTargetEl      <- UI.div # set style rmTargetStyle
    let closeRMTarget =
          element rmTargetEl # set style [("width", "0"), ("height", "0")]
    (menuEl, closeMenu, closeNestedMenus) <- newMenu [closeRMTarget] items
    -- Define functions to open and close the menu.
    let openMenu (x, y) = do
          element menuEl # set style
            [("left", show x ++ "px"), ("top", show y ++ "px"),
             ("display", "block")]
          element rmTargetEl # set style
            [("width", "100vw"), ("height", "100vh")]
    -- Display menu on a contextmenu event.
    on UI.contextmenu source $ \(x, y) -> do
      liftIO $ putStrLn "context event fired"
      openMenu (x, y)
    -- Hide menu on rmTarget click.
    on UI.mousedown rmTargetEl $ const $ do
      closeMenu >> sequence closeNestedMenus
      liftIO $ putStrLn "rmTarget clicked"
    -- Hide nested menus on hover over rmTarget.
    on UI.hover rmTargetEl $ const $ sequence_ closeNestedMenus 
    element source #+ [element rmTargetEl, element menuEl]
    preventDefaultContextMenu source

-- | Returns a menu element and a function to close the menu and any nested
--   menus.
newMenu :: [Action] -> [MenuItem a] -> UI (Element, Action, [Action])
newMenu closeParents menuItems = do
    menuEl <- UI.li # set style menuStyle
    let closeMenu = display "none" menuEl
    -- Menu items as elements and respective list of actions to close nested
    -- menus. :: UI [(Element, [Action])]
    menuItemEls <- mapM (menuItem $ closeParents ++ [closeMenu]) menuItems 
    element menuEl #+ map (element . fst) menuItemEls
    --  indexedCloseNested :: [((Element, [Action]), Integer)]
    let indexedCloseNested = zip menuItemEls [1..]
        -- On hover over a menu item we want close any nested menus from
        -- *other* menu items. To do this we map the following function over
        -- all menu items. This function takes a menu item from the list and
        -- when it is hovered over will run the close actions of all *other*
        -- menu items.
        closeOtherMenusOnHover ((el1, _), index1) =
            on UI.hover el1 $ const $ do
                let closeIfNotSelf ((_, closeOther), index2) = do
                      liftIO $ putStrLn $ show index1 ++ show index2
                      when (index1 /= index2) (sequence_ closeOther)
                mapM closeIfNotSelf indexedCloseNested
    mapM closeOtherMenusOnHover indexedCloseNested
    return (menuEl, closeMenu, concat (map snd menuItemEls))

-- | Returns a menu item element and actions to open and close it.
menuItem :: [Action] -> MenuItem a -> UI (Element, [Action])
menuItem closeAbove (MenuItem text value) = do
    menuItemEl <- UI.li # set UI.text text # set style menuItemStyle
    highlightWhileHover menuItemEl
    case value of
        MenuItemActions actions -> do
            -- On click execute the actions and close the entire menu.
            on UI.click menuItemEl $ const $ do
                sequence_ $ closeAbove ++ actions
                liftIO $ putStrLn "event clicked"
            return (menuItemEl, [])
        NestedMenu nestedMenuItems -> do
            (nestedMenuEl, closeMenu, closeNestedMenu)
                 <- newMenu closeAbove nestedMenuItems
            element menuItemEl #+ [element nestedMenuEl]
            -- On hover display the nested menu.
            on UI.hover menuItemEl $ const $ display "block" nestedMenuEl
            return (menuItemEl, [closeMenu] ++ closeNestedMenu)

-- | Sets the CSS "display: X;" on the given element.
display :: String -> Element -> UI Element
display x el = element el # set style [("display", x)]

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
