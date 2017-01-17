module ContextMenu where

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
newMenu :: UI Element -> [MenuItem a] -> UI (Element, UI Element)
newMenu closeParent menuItems = do
    -- Create a blank menu and function to close it.
    menuEl <- UI.li # set style menuStyle
    let close = element menuEl # set style [("display", "none")]
    -- Append all menu items to the menu.
    menuItemEls <- mapM (menuItem close) menuItems
    element menuEl #+ map element menuItemEls
    return (menuEl, close)

-- |Returns a menu item element and potentially a function to close a submenu.
menuItem :: UI Element -> MenuItem a -> UI Element
menuItem closeRoot (MenuItem text value) = do
    menuItemEl <- UI.li # set UI.text text # set style menuItemStyle
    -- Change menu item appearance on hover.
    whileHover menuItemEl 
        (element menuItemEl # set style [("background-color", "#DEF"   )])
        (element menuItemEl # set style [("background-color", "inherit")])
    case value of
        -- On click of menu item with actions, execute the actions and close
        -- the entire menu.
        MenuItemActions actions ->
            on UI.click menuItemEl $ const $ do
                closeRoot
                liftIO $ putStrLn "event clicked"
                sequence_ actions
        -- On hover over menu item with nested menu, display the nested menu.
        NestedMenu nestedMenuItems -> do
            (nestedMenuEl, closeNested) <- newMenu closeRoot nestedMenuItems
            element menuItemEl #+ [element nestedMenuEl]
            on UI.hover menuItemEl $ const $
                element nestedMenuEl # set style [("display", "block")]
            -- whileHover nestedMenuEl openNested closeNested
        -- (subMenu, closeMenu) <- newMenu lm
            return ()
    return menuItemEl

-- | Execute one action on hover and another on leave.
whileHover :: Element -> UI a -> UI b -> UI ()
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
    runFunction $ ffi
        "$(%1).bind('contextmenu', e => e.preventDefault())"
        ("." ++ preventDefaultClass)
