module Handler.Home
    ( getHomeR
    ) where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Try Cryptography"
        $(widgetFile "homepage")
