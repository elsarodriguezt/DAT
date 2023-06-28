{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
where
import Develop.DatFw
import Develop.DatFw.Template
import Network.Wai.Handler.Warp (run)

import System.Environment (getArgs)

-- Definicions basiques del lloc

data HelloWorld = HelloWorld

instance WebApp HelloWorld

-- Rutes i 'dispatch'

instance RenderRoute HelloWorld where
    data Route HelloWorld = HomeR
    renderRoute HomeR = ([], [])

instance Dispatch HelloWorld where
    dispatch =
        routing $ route ( onStatic [] ) HomeR [ onMethod "GET" getHomeR ]

-- 'Handlers'

getHomeR :: HandlerFor HelloWorld Html
getHomeR = defaultLayout $ do
    setTitle "Hello"
    [widgetTempl| <h1>Hello World!</h1> |]

-- Inicialització

main :: IO ()
main = do
    args <- getArgs
    print args
    let port = read $ head args
    app <- toApp HelloWorld
    -- Warp adapter:  run :: Application -> IO ()
    run port app
