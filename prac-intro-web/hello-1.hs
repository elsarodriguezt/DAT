
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (runEnv)
import           Web.Cookie

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8

import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Data.Monoid
import           Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- ****************************************************************

main :: IO ()
main = do
    -- runEnv :: Port -> Application -> IO ()
    runEnv 4050 helloApp

-- ****************************************************************

helloApp :: Application
helloApp req respond = do -- Monad IO
    let mbSessionName = requestCookieValue "user-name" req
    case requestMethod req of
        "GET" -> do
            respond $
                responseBuilder
                        ok200
                        [("Content-Type", "text/html;charset=utf-8")]
                        (renderHtmlBuilder $ htmlView mbSessionName Nothing)
        "POST" -> do
            query <- requestPostQuery req
            let euserName = do -- Monad (Either T.Text)
                    value <- maybe (Left "Nom obligatori") (Right . T.decodeUtf8) $
                                lookup "user-name" query
                    if T.null value then Left "Nom obligatori"
                    else if T.length value < 5 then Left "Nom massa curt"
                    else Right value
            case euserName of
                Left err ->
                    respond $
                        responseBuilder
                            ok200
                            [("Content-Type", "text/html;charset=utf-8")]
                            (renderHtmlBuilder $ htmlView mbSessionName $ Just err)
                Right userName ->
                    respond $
                        responseBuilder
                            seeOther303
                            [("Location", "#"), ("Set-Cookie", mkSetCookieValue "user-name" userName)]
                            mempty
        _ ->
            respond $
                responseBuilder
                        methodNotAllowed405
                        [("Content-Type", "text/plain")]
                        (stringUtf8 "Invalid method")


requestPostQuery :: Request -> IO SimpleQuery
requestPostQuery req =
    if lookup hContentType (requestHeaders req) == Just "application/x-www-form-urlencoded" then
        parseSimpleQuery <$> getBody req
    else
        pure mempty
    where
        getBody req = do
            b <- getRequestBodyChunk req
            if B.null b then pure B.empty
            else do
                bs <- getBody req
                pure $ b <> bs

-- Obte l'estat de sessio a partir de la corresponent 'cookie' de la peticio.
requestCookieValue :: T.Text -> Request -> Maybe T.Text
requestCookieValue name req = do
    cookieHeader <- lookup "Cookie" (requestHeaders req)
    T.decodeUtf8 <$> lookup (T.encodeUtf8 name) (parseCookies cookieHeader)

-- Funcio auxiliar que obte el valor de la 'cookie' resultant a partir de l'estat de sessio.
mkSetCookieValue :: T.Text -> T.Text -> B.ByteString
mkSetCookieValue name value =
    let setCookie = defaultSetCookie { setCookieName = T.encodeUtf8 name
                                     , setCookieValue = T.encodeUtf8 value
                                     }
    in BL.toStrict $ toLazyByteString $ renderSetCookie setCookie

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

