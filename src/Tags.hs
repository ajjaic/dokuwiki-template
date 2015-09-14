module Tags
    ( h1
    , h2
    ) where

h1 :: String
h1 = "====== Level1 ======"

h2 :: String
h2 = "===== Level2 ====="

{-imgInternal :: ImageLink-}
{-imgInternal = Img $ Internal "[[namespace:pagename|{{wiki:yourpicture.png}}]]"-}

{-imgExternal :: ImageLink-}
{-imgExternal = Img $ External "[[http://yourlink.com|{{wiki:yourpicture.png}}]]"-}

{-urlInternalPlain :: URLLink-}
{-urlInternalPlain = URL $ Internal "[[pagename|text to show]]"-}

{-urlInternalNamespace :: URLLink-}
{-urlInternalNamespace = URL $ Internal "[[namespace:pagename|text to show]]"-}

{-urlInternalSection :: URLLink-}
{-urlInternalSection = URL $ Internal "[[namespace:pagename#section|text to show]]"-}

{-urlExternal :: URLLink-}
{-urlExternal = URL $ External "[[http://yourlink.com|yourlink]]"-}
