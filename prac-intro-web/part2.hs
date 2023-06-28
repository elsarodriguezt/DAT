{-# LANGUAGE OverloadedStrings #-}

module Main
where
import           Network.Wai
import           Network.Wai.Handler.Warp (runEnv)

import           Handler

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- ****************************************************************

main :: IO ()
main = do
    -- runEnv :: Port -> Application -> IO ()
    runEnv 4050 $ dispatchHandler gameApp

-- ****************************************************************
-- Controller

gameApp :: Handler HandlerResponse
gameApp = onMethod
    [ ("GET", doGet)
    , ("POST", doPost)
    ]

doGet :: Handler HandlerResponse
doGet = ...

doPost :: Handler HandlerResponse
doPost = ...

-- ****************************************************************
-- View

htmlView :: GameState -> H.Html
htmlView game =
    H.docTypeHtml $ do
        H.head $
            H.title "A simple game ..."
        H.body $ do
            H.h1 $ H.text $ "Game state: " <> T.pack (show game)
            H.hr
            H.form H.! A.method "POST" H.! A.action "#" $ do
                H.p $ do
                    H.span "String to play:"
                    H.input H.! A.name "playText"
                H.input H.! A.type_ "submit" H.! A.name "ok" H.! A.value "Play"

-- ****************************************************************
-- Model

-- Tipus de l'estat
type GameState = (Bool, Int)

startState :: GameState
startState = (False, 0)

playChar :: Char -> GameState -> GameState
playChar ...

playString :: String -> GameState -> GameState
playString ...

playText :: Text -> GameState -> GameState
playText t = playString (T.unpack t)
