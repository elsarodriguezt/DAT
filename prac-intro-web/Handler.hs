
{-# LANGUAGE OverloadedStrings #-}

module Handler
    -- Exporta les seguents declaracions d'aquest modul
    ( Handler, dispatchHandler
    , HandlerResponse, respHtml, respRedirect, respError
    , getMethod, onMethod
    , getSession, setSession, deleteSession
    , postParams, lookupPostParams, lookupPostParam
    )
where
import qualified Network.Wai as W
import qualified Network.HTTP.Types as W
import qualified Web.Cookie as W

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html.Renderer.Utf8

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import           Text.Read

-- ****************************************************************

-- Tipus correponent al monad 'Handler'.
-- El context d'un Handler compren:
--      L'argument Request que permet obtenir informacio sobre la peticio.
--      L'estat del Handler (argument i resultat de les operacions).
newtype Handler a =
    HandlerC { runHandler :: W.Request -> HandlerState -> IO (a, HandlerState) }

-- Aquesta funció pren una petició web (W.Request) i un estat de Handler (HandlerState) i retorna una acció d'entrada/sortida (IO) que produeix un resultat de tipus a i un nou estat de manipulador (HandlerState).

-- HandlerState compren:
--      'Cache' dels parametres de la peticio.
--      L'estat de la sessio que s'obte de les corresponents 'cookies'.
--        Aquest estat de sessio es una llista de parelles nom-valor.

data HandlerState =
    HandlerStateC { hsQuery :: Maybe [(Text, Text)], hsSession :: [(Text, Text)] }

-- Representa l'estat del Handler. Té dos camps: hsQuery és un valor opcional de tipus Maybe [(Text, Text)] que conté els paràmetres de la petició, i hsSession és una llista de parelles clau-valor ([(Text, Text)]) que representa l'estat de la sessió.


-- Funcions auxiliars per modificar l'estat del handler:

hsSetQuery :: Maybe [(Text, Text)] -> HandlerState -> HandlerState
-- hsSetQuery pren un valor opcional de paràmetres de la petició (Maybe [(Text, Text)]) i un estat del Handelr (HandlerState) com a arguments. Retorna un nou estat del Handler amb el camp hsQuery actualitzat amb el valor proporcionat.

hsSetQuery q (HandlerStateC _ s) = HandlerStateC q s
-- La funció hsSetQuery pren un valor q (valor opcional de paràmetres de la petició) i un estat del Handler HandlerStateC _ s. Ignora el primer camp de l'estat actual (_) i agafa el segon camp (s) com a part de l'estat actualitzat. Crea un nou estat HandlerStateC amb el valor de q com a nou camp hsQuery i el mateix valor de s com a camp hsSession. Això retorna el nou estat de manipulador amb el camp hsQuery actualitzat amb el valor proporcionat.

hsSetSession :: [(Text, Text)] -> HandlerState -> HandlerState
-- hsSetSession pren una llista de parelles clau-valor ([(Text, Text)]) que representa l'estat de la sessió i un estat del manipulador (HandlerState) com a arguments. Retorna un nou estat del manipulador amb el camp hsSession actualitzat amb el valor proporcionat.

hsSetSession s (HandlerStateC q _) = HandlerStateC q s
-- La funció hsSetSession pren un valor s (estat de la sessió) i un estat de manipulador HandlerStateC q _. Ignora el segon camp de l'estat actual (_) i agafa el primer camp (q) com a part de l'estat actualitzat. Crea un nou estat de manipulador HandlerStateC amb el mateix valor de q com a camp hsQuery i el valor de s com a nou camp hsSession. Això retorna el nou estat de manipulador amb el camp hsSession actualitzat amb el valor proporcionat.


instance Functor Handler where -- instància de Functor per al tipus de dades Handler = podem aplicar la funció fmap a valors de tipus Handler.
    -- tipus en aquesta instancia:
    --      fmap :: (a -> b) -> Handler a -> Handler b
    fmap f (HandlerC h) = HandlerC $ \ req st0 -> do
        -- Monad IO:
        (x, st1) <- h req st0
        pure (f x, st1) -- return
-- La funció pren una funció f i un valor HandlerC h. Descompon el valor HandlerC h en la funció h. Llavors, crea una nova funció HandlerC que pren una petició req i un estat inicial st0 com a arguments. Llavors, executa la funció h amb la petició i l'estat inicial per obtenir una acció d'entrada/sortida (IO) que produeix un resultat x i un nou estat st1. Finalment, retorna una acció IO que empaqueta el resultat de f x i el nou estat st1.
-- Pure: Ens permet introduir un valor en el context de Handler sense alterar l'estat del manipulador.

instance Applicative Handler where --instància de Applicative per al tipus de dades Handler = podem utilitzar les funcions pure i <*> amb valors de tipus Handler.

    -- tipus en aquesta instancia:
    --      pure  :: a -> Handler a
    --      (<*>) :: Handler (a -> b) -> Handler a -> Handler b
    pure x =
        -- (A completar per l'estudiant)
        HandlerC $ \req s0 -> pure (x, s0)
-- La funció pren un valor x i retorna una nova funció HandlerC que pren una petició req i un estat inicial s0 com a arguments. Llavors, utilitza la funció pure per empaquetar el valor x i l'estat inicial s0 en una acció IO i la retorna.
    
    HandlerC hf <*> HandlerC hx =
        -- (A completar per l'estudiant)
        HandlerC $ \req s0 -> do
            (f,s1) <- hf req s0
            (x,s2) <- hx req s1
            pure (f x, s2)
-- La funció pren dues funcions HandlerC hf i HandlerC hx. Descompon aquestes dues funcions en hf i hx. Llavors, crea una nova funció HandlerC que pren una petició req i un estat inicial s0 com a arguments.
-- Llavors, executa la funció hf amb la petició i l'estat inicial per obtenir una acció d'entrada/sortida (IO) que produeix una funció f i un nou estat s1. 
--A continuació, executa la funció hx amb la mateixa petició i l'estat s1 per obtenir una acció d'entrada/sortida (IO) que produeix un valor x i un nou estat s2. 
--Finalment, s'utilitza la funció pure per encapsular el resultat de f x (l'aplicació de la funció f al valor x) i el nou estat s2 en una acció IO. Això retorna un nou valor Handler que encapsula el resultat de l'aplicació de la funció f a x i el nou estat s2


instance Monad Handler where -- instància de Monad = Handler és un monad, que és una estructura de programació que permet combinar operacions seqüencialment i manipular els seus resultats.

    -- tipus en aquesta instancia:
    --      (>>=) :: Handler a -> (a -> Handler b) -> Handler b
    HandlerC hx >>= f = 
    
    -- declara la definició de l'operador de vinculació (>>=) per a la instància Monad de Handler. Aquest operador ens permet combinar valors Handler i aplicar funcions a aquests valors.
    
    
        -- (A completar per l'estudiant)
        HandlerC $ \req s0 -> do -- crea una nova funció HandlerC que pren una sol·licitud (req) i un estat inicial (s0) com a arguments.
           (x,s1) <- hx req s0 -- es descompon el valor hx en (x,s1) utilitzant el patró (x,s1) <- hx req s0. Això executa l'acció encapsulada hx amb la sol·licitud req i l'estat inicial s0, obtenint un resultat (x,s1). L'acció encapsulada hx és una funció que retorna un valor a i un nou estat s1.
           let HandlerC hy = f x -- es defineix una nova funció HandlerC utilitzant let HandlerC hy = f x. Aquesta línia aplica la funció f al valor x i descompon el resultat en la funció hy.
           hy req s1 -- s'executa l'acció encapsulada hy amb la mateixa sol·licitud req i l'estat s1. Això produeix un nou valor b i un estat actualitzat s2.


-- class MonadIO: Monads in which IO computations may be embedded.
-- The method 'liftIO' lifts a computation from the IO monad.
instance MonadIO Handler where
    -- tipus en aquesta instancia:
    --      liftIO :: IO a -> Handler a
    liftIO io = HandlerC $ \ _ st0 -> do
        x <- io
        pure (x, st0)
-- la instància MonadIO per al tipus de dades Handler permet utilitzar la funció liftIO per encapsular una acció d'entrada/sortida IO en un valor de tipus Handler. Aquesta instància permet combinar funcionalitats d'entrada/sortida amb el monad Handler, proporcionant una manera de treballar amb operacions d'entrada/sortida dins del context de Handler.

-- ****************************************************************
-- Aquestes funcions no s'exporten pero son utils en les implementacions
-- de les funcions exportades.

-- Obte informació de la peticio
asksRequest :: (W.Request -> a) -> Handler a
asksRequest f = HandlerC $ \req st0 ->
    pure (f req, st0)
-- La funció asksRequest retorna un valor de tipus Handler a que encapsula una acció. Aquesta acció consisteix a obtenir el W.Request actual i aplicar la funció f a aquest request. El resultat de la funció f s'empaqueta dins del valor Handler a.


-- Obte informació de l'estat del handler.Permet obtenir l'estat actual del handler i aplicar una funció f a aquest estat, retornant el resultat empaquetat en un valor de tipus Handler a
getsHandlerState :: (HandlerState -> a) -> Handler a
getsHandlerState f =
    -- (A completar per l'estudiant)
    HandlerC $ \req s0 -> pure (f s0, s0)
    
    
-- Utilitza un constructor de tipus HandlerC per construir un valor de tipus Handler a. Aquest constructor pren una funció com a argument, la qual té dos paràmetres: req i s0. Aquesta funció encapsula l'acció que s'executarà quan es cridi el valor de tipus Handler a.Amb pure empaquetem el resultat de cridar la funció f amb l'estat s0 en una estructura monàdica. En aquest cas, l'estructura monàdica que encapsula una tupla que conté el resultat de f s0 i l'estat s0.   
    



-- Modifica l'estat del handler
modifyHandlerState :: (HandlerState -> HandlerState) -> Handler ()
modifyHandlerState f =
    -- (A completar per l'estudiant)
    HandlerC $ \ req s0 -> pure ((), f s0)

-- HandlerC rep una funció com a argument, que té dos paràmetres: req i s0. Aquesta funció encapsula l'acció que s'executarà quan es cridi el valor de tipus Handler ().Amb pure empaquetem el valor (), aquest valor () es col·loca juntament amb el resultat de cridar la funció f amb l'estat s0 en una tupla, i aquesta tupla és retornada com a resultat de l'acció encapsulada en el valor Handler ().



-- ****************************************************************

-- Tipus que ha de tenir el resultat del handler que se li passa a 'dispatchHandler'.
data HandlerResponse =
        HRHtml H.Html           -- Resposta normal. Parametre: Contingut HTML.
      | HRRedirect Text         -- Redireccio. Parametre: URL.
      | HRError W.Status Text   -- Resposta anormal. Parametres: Codi d'estat HTTP i missatge.

-- 'dispatchHandler' converteix (adapta) un 'Handler' a una aplicacio WAI,
-- realitzant els passos seguents:
--      Obte l'estat inicial (st0) del handler amb una sessio inicial a partir
--        de les cookies rebudes en la peticio WAI.
--      Executa el handler passant-li la peticio i l'estat inicial.
--      Amb l'execucio del handler s'obte el parell format
--        pel resultat del handler (res) i l'estat final (st1).
--      Construeix la corresponent resposta WAI i l'envia.
--        La resposta WAI depen del nou estat de sessio en st1.
-- El tipus 'Application' esta definit en el modul 'Network.Wai' com:
--      type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
dispatchHandler :: Handler HandlerResponse -> W.Application
dispatchHandler handler req respond = do
    -- Monad IO:
    let st0 = HandlerStateC{ hsQuery = Nothing, hsSession = requestSession req }
    (res, st1) <- runHandler handler req st0
    let scValue = mkSetCookieValue $ hsSession st1
        wairesp = case res of
            HRHtml html ->
                let headers = [ ("Content-Type", mimeHtml)
                              , ("Set-Cookie", scValue) ]
                in W.responseBuilder W.ok200 headers (renderHtmlBuilder html)
            HRRedirect url ->
                let headers = [ ("Location", T.encodeUtf8 url)
                              , ("Content-Type", mimeText)
                              , ("Set-Cookie", scValue) ]
                in W.responseBuilder W.seeOther303 headers (T.encodeUtf8Builder "Redirect")
            HRError status msg ->
                let headers = [ ("Content-Type", mimeText) ]
                in W.responseBuilder status headers (T.encodeUtf8Builder msg)
    respond wairesp

-- Els constructors de HandlerResponse no s'exporten.
-- S'exporten en canvi les funcions seguents que obtenen simples handlers que retornen
-- els diferents tipus de resposta:

respHtml :: H.Html -> Handler HandlerResponse
respHtml html = pure $ HRHtml html

respRedirect :: Text -> Handler HandlerResponse
respRedirect url = pure $ HRRedirect url

respError :: W.Status -> Text -> Handler HandlerResponse
respError status msg = pure $ HRError status msg


-- ****************************************************************

-- Obte el metode HTTP de la peticio
getMethod :: Handler W.Method
getMethod = asksRequest W.requestMethod

-- Obte el metode HTTP de la peticio
onMethod :: [(W.Method, Handler HandlerResponse)] -> Handler HandlerResponse
onMethod alts = do
    -- Monad Handler:
    method <- getMethod
    case lookup method alts of
        Just h -> h
        Nothing -> respError W.methodNotAllowed405 "Invalid method"

-- Obte el valor de l'atribut de sessio amb el nom indicat.
-- Retorna Nothing si l'atribut indicat no existeix o no te la sintaxis adequada.
getSession :: Read a => Text -> Handler (Maybe a)
getSession name = do
    session <- getsHandlerState hsSession
    pure $ maybe Nothing (readMaybe . T.unpack) $ lookup name session

-- Fixa l'atribut de sessio amb el nom i valor indicats.
setSession :: Show a => Text -> a -> Handler ()
setSession name value = do
    session <- getsHandlerState hsSession
    let newsession = (name, T.pack $ show value) : filter ((name /=) . fst) session
    modifyHandlerState (hsSetSession newsession)

-- Elimina l'atribut de sessio amb el nom indicat.
deleteSession :: Text -> Handler ()
deleteSession name = do
    session <- getsHandlerState hsSession
    let newsession = filter ((name /=) . fst) session
    modifyHandlerState (hsSetSession newsession)

-- Obte els valor associat al parametre de la peticio amb el nom indicat.
lookupPostParam :: Text -> Handler (Maybe Text)
lookupPostParam name = do
    vals <- lookupPostParams name
    case vals of
        [] -> pure Nothing
        (v:_) -> pure (Just v)

-- Obte els valors associats al parametre de la peticio amb el nom indicat.
lookupPostParams :: Text -> Handler [Text]
lookupPostParams name = do
    -- Monad Handler:
    mbparams <- postParams
    case mbparams of
        Just params -> -- params es una llista de parelles de tipus (Text, Text)
            -- Caldra obtenir tots els valors (segon component) de les parelles que tenen el nom (primer component) igual al indicat.
            -- NOTA: Useu les funcions
            --   fst :: (a, b) -> a
            --   snd :: (a, b) -> b
            --   filter :: (a -> Bool) -> [a] -> [a]
            -- (A completar per l'estudiant)
            return (map snd(filter (\param -> fst param == name) params))
        Nothing ->
            -- El contingut de la peticio no es un formulari. No hi ha valors.
            pure []

-- Obte tots els parametres (parelles (nom,valor)) del contingut de la peticio.
-- Retorna Nothing si el contingut de la peticio no es un formulari.
postParams :: Handler (Maybe [(Text, Text)])
postParams = do
    -- Si previament ja s'havien obtingut els parametres (i guardats en l'estat del handler)
    -- aleshores es retornen aquests, evitant tornar a llegir el contingut de la peticio.
    cache <- getsHandlerState hsQuery
    case cache of
        Just query ->
            pure $ Just query
        Nothing -> do
            req <- asksRequest id
            if lookup W.hContentType (W.requestHeaders req) == Just "application/x-www-form-urlencoded" then do
                query <- liftIO $ parsePostQuery <$> getAllBody req
                modifyHandlerState (hsSetQuery $ Just query)
                pure $ Just query
            else
                pure Nothing

-- ****************************************************************
-- Funcions internes (utilitats no exportades)

mimeText :: B.ByteString
mimeText = "text/plain;charset=UTF-8"

mimeHtml :: B.ByteString
mimeHtml = "text/html;charset=UTF-8"

-- Obte l'estat de sessio a partir de la corresponent 'cookie' de la peticio.
requestSession :: W.Request -> [(Text, Text)]
requestSession req =
    let mbvalue = do -- Monad Maybe
            cookieHeader <- lookup "Cookie" (W.requestHeaders req)
            session <- lookup (T.encodeUtf8 "session") (W.parseCookies cookieHeader)
            readMaybe $ T.unpack $ T.decodeUtf8 session
    in maybe [] id mbvalue

-- Funcio auxiliar que obte el valor de la 'cookie' resultant a partir de l'estat de sessio.
mkSetCookieValue :: [(Text, Text)] -> B.ByteString
mkSetCookieValue session =
    let setCookie = W.defaultSetCookie { W.setCookieName = T.encodeUtf8 "session"
                                       , W.setCookieValue = T.encodeUtf8 $ T.pack $ show session
                                       }
    in BL.toStrict $ toLazyByteString $ W.renderSetCookie setCookie

parsePostQuery :: B.ByteString -> [(Text, Text)]
parsePostQuery content =
    decodepair <$> W.parseSimpleQuery content
    where
        decodepair (n, v) = (T.decodeUtf8 n, T.decodeUtf8 v)

getAllBody :: W.Request -> IO B.ByteString
getAllBody req = do
    b <- W.getRequestBodyChunk req
    if B.null b then pure B.empty
    else do
        bs <- getAllBody req
        pure $ b <> bs

