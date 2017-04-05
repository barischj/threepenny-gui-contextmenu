-- This module could potentially be rewritten and heavily simplified once the
-- custom context menu spec in HTML 5.1 is adopted by all major browsers. At
-- time of writing the spec is only a recommendation and only implemented by
-- Mozilla Firefox.

module Graphics.UI.Threepenny.Ext.Contextmenu where

import           Control.Monad                                (when)
import qualified Graphics.UI.Threepenny                       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Ext.Contextmenu.Style
import           Graphics.UI.Threepenny.Ext.Contextmenu.Util

-- |A menu item is some text to be displayed and either UI actions to execute or
-- a nested menu.
data MenuItem a = MenuItem { mIText :: String, mIValue :: MenuItemValue a }
data MenuItemValue a = MenuItemActions [Action] | NestedMenu [MenuItem a]

-- |Constructor for a menu item that contains UI actions to execute.
actionMenuItem :: String -> [Action] -> MenuItem a
actionMenuItem text actions =
    MenuItem { mIText = text, mIValue = MenuItemActions actions }

-- |Constructor for a menu item that contains a nested menu.
nestedMenuItem :: String -> [MenuItem a] -> MenuItem a
nestedMenuItem text nested =
    MenuItem { mIText = text ++ "  ›", mIValue = NestedMenu nested }

-- |Creates a custom context menu, activated from the given source element and
-- attached to the HTML body. Prevents the default context menu from occuring.
contextMenu :: [MenuItem a] -> Element -> UI ()
contextMenu items sourceEl = do
    rmTargetEl <- UI.div # set style rmTargetStyle
    let closeRmTarget = dimensions "0" "0" rmTargetEl
    (menuEl, closeMenu, closeNestedMenus) <- newMenu [closeRmTarget] items
    -- Display menu on a contextmenu event.
    on UI.contextmenu sourceEl $ \(x, y) ->
        displayAt x y menuEl >> dimensions "100vw" "100vh" rmTargetEl
    -- Hide everything on rmTarget click.
    on UI.mousedown rmTargetEl $ const $
        closeRmTarget >> closeMenu >> sequence closeNestedMenus
    -- Hide nested menus on hover over rmTarget.
    on UI.hover rmTargetEl $ const $ sequence closeNestedMenus
    -- Attach everything to the body, with a large z-index.
    parent <- UI.div #+ [element rmTargetEl, element menuEl]
                     # set UI.style [("z-index", "10000"),
                                     ("position", "absolute")]
    (askWindow >>= getBody) #+ [element parent]
    preventDefaultContextMenu sourceEl

-- |Returns a menu element, an action to close the menu and actions to close any
-- nested menus.
newMenu :: [Action] -> [MenuItem a] -> UI (Element, Action, [Action])
newMenu closeParents menuItems = do
    menuEl <- UI.li # set style menuStyle
    let closeMenu = display "none" menuEl
    -- Menu items as elements and respective list of actions to close nested
    -- menus. :: UI [(Element, [Action])]
    menuItemEls <- mapM (menuItem $ closeParents ++ [closeMenu]) menuItems
    element menuEl #+ map (element . fst) menuItemEls
        -- On hover over a menu item we want close any nested menus from
        -- *other* menu items.
    let closeOtherMenusOnHover ((el1, _), i1) xs =
          on UI.hover el1 $ const $ do
              let closeIfNotSelf ((_, closeEl2), i2) =
                    when (i1 /= i2) (sequence_ closeEl2)
              mapM closeIfNotSelf xs
    mapPairsWithIndex menuItemEls closeOtherMenusOnHover
    return (menuEl, closeMenu, concat (map snd menuItemEls))

-- |Returns a menu item element and actions to open and close it.
menuItem :: [Action] -> MenuItem a -> UI (Element, [Action])
menuItem closeAbove (MenuItem text value) = do
    menuItemEl <- UI.li # set UI.text text # set style menuItemStyle
    highlightWhileHover menuItemEl
    case value of
        MenuItemActions actions -> do
            -- On click close the entire menu and execute the actions.
            on UI.click menuItemEl $ const $ sequence $ closeAbove ++ actions
            return (menuItemEl, [])
        NestedMenu nestedMenuItems -> do
            (nestedMenuEl, closeMenu, closeNestedMenu)
                 <- newMenu closeAbove nestedMenuItems
            -- Position a nested menu relative to this menu item.
            -- element menuItemEl # set UI.position "relative"
            -- element nestedMenuEl # set UI.position "absolute" # set UI.right "0px" # set UI.top "0px"
            element menuItemEl #+ [element nestedMenuEl]
            -- On hover display the nested menu.
            on UI.hover menuItemEl $ const $ display "block" nestedMenuEl
            return (menuItemEl, [closeMenu] ++ closeNestedMenu)

-- |Highlight a given element while it is hovered over.
highlightWhileHover :: Element -> UI ()
highlightWhileHover el = whileHover el
    (element el # set style [("background-color", "#DEF"   )])
    (element el # set style [("background-color", "inherit")])

-- |CSS class used to identify elements on which to prevent a default context
-- menu from opening.
preventDefaultClass = "__prevent-default-context-menu"

-- |Prevents a default context menu opening from the given element.
preventDefaultContextMenu :: Element -> UI ()
preventDefaultContextMenu el = do
    element el # set UI.class_ preventDefaultClass
    runFunction $ ffi "$(%1).bind('contextmenu', e => e.preventDefault())"
                      ("." ++ preventDefaultClass)

