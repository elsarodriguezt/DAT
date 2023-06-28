
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import           Network.Wai
import           Network.Wai.Handler.Warp (runEnv)

import           Handler

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

-- ****************************************************************

main :: IO ()
main = do
    -- runEnv :: Port -> Application -> IO ()
    runEnv 4050 $ dispatchHandler helloApp

-- ****************************************************************

helloApp :: Handler HandlerResponse
helloApp = do -- Monad Handler
    mbSessionName <- getSession "user-name"
    onMethod
        [("GET", respHtml $ htmlView mbSessionName Nothing)
        ,("POST", do
            mbvalue <- lookupPostParam "user-name"
            let euserName = do -- Monad (Either T.Text)
                    value <- maybe (Left "Nom obligatori") Right mbvalue
                    if T.null value then Left "Nom obligatori"
                    else if T.length value < 5 then Left "Nom massa curt"
                    else Right value
            case euserName of
                Left err ->
                    respHtml $ htmlView mbSessionName $ Just err
                Right userName -> do
                    setSession "user-name" userName
                    respRedirect "#"
         )]

htmlView :: Maybe T.Text -> Maybe T.Text -> H.Html
htmlView mbnom mberr =
    H.docTypeHtml $ do
        H.head $
            H.title "Hello ..."
        H.body $ do
            case mbnom of
                Just nom -> H.h1 $ H.text $ "Hola " <> nom
                Nothing -> mempty
            H.hr
            H.form H.! A.method "POST" H.! A.action "#" $ do
                H.p $ do
                    H.span "Nom:"
                    H.input H.! A.name "user-name"
                    case mberr of
                        Just err -> H.div H.! A.style "color:red" $ H.text err
                        Nothing -> mempty
                H.input H.! A.type_ "submit" H.! A.name "ok" H.! A.value "Ok"

