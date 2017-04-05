module Graphics.UI.Threepenny.Ext.Contextmenu.Style where

-- |Default style for the context menu.
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

-- |Default style for any menu items.
menuItemStyle = [
        ("cursor",  "pointer"),
        ("padding", "8px 12px")
    ]

-- |Full-screen transparent target to close the menu.
rmTargetStyle = [
        ("height",   "0"),
        ("left",     "0"),
        ("position", "absolute"),
        ("top",      "0"),
        ("width",    "0")
    ]
