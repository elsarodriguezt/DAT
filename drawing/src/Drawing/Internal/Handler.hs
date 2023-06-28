
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(WAI_CGI) || defined(WAI_WARP)
#define WAI
#else
#error "Which handler ?"
#endif

module Drawing.Internal.Handler
where
import Drawing.Internal.Types
import Drawing.Internal.Render (unitSize)
import Drawing.Internal.Handler.Store

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Middleware.Static as MwSt
#if defined(WAI_CGI)
import Network.Wai.Handler.CGI (run)
---import Network.Wai.Handler.Launch
#elif defined(WAI_WARP)
import Network.Wai.Handler.Warp (runEnv)
#endif

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder as Bd

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encoding

import Text.Read
import Data.Time

import Control.Exception
import Control.Monad
import System.Environment
import System.Exit (exitSuccess, die)
import System.Console.GetOpt

import qualified Paths_drawing as Paths


#ifndef WAI
--- !defined(MIN_VERSION) || !WAI(3,0,0)
responseBuilder :: Status -> [Header] -> Bd.Builder -> Response
responseBuilder status hs zbd =
    error "TODO"
    ---ResponseBuilder status hs Bd.lazyByteString
#endif


-- ---------------------------------------------------------------
-- Main entry points
-- ---------------------------------------------------------------

-- | A simple drawing CGI program.
drawingOf :: Double             -- ^ The width (in drawing units) of the view.
          -> Double             -- ^ The height (in drawing units) of the view.
          -> Drawing
          -> IO ()
drawingOf width height draw =
    activityOf width height () (const id) (const draw)

-- | A CGI program which shows an animation, with a picture for each time given by the parameter.
animationOf :: Double                   -- ^ The width (in drawing units) of the view.
            -> Double                   -- ^ The height (in drawing units) of the view.      
            -> (Time -> Drawing)        -- ^ Function to produce the next frame of animation.
                                        --   It is passed the time in seconds since the program started.
            -> IO ()
animationOf width height view =
    activityOf width height 0.0 input view
    where
        input (TimePassing dt) = (dt +)
        input _ = id

-- | An interactive CGI program that responds to Events.
-- Activities can interact with the user, change over time, and remember information about the past.
activityOf :: Double            -- ^ The width (in drawing units) of the view.
           -> Double            -- ^ The height (in drawing units) of the view.
           -> a                 -- ^ The initial state of the activity.
           -> (Event -> a -> a) -- ^ The event handling function, which updates the state given an event.
           -> (a -> Drawing)    -- ^ The visualization function, which converts the state into a picture to display.
           -> IO ()
activityOf width height sate0 input view =
    activityOf' $ Env (round $ width * unitSize / 2) (round $ height * unitSize / 2) sate0 input view

activityOf' :: Env a -> IO ()
activityOf' env = do
    flags <- getArgs >>= parseArguments
    ddir <- Paths.getDataDir
    putStrLn $ "DEBUG: getDataDir: " <> ddir
    Paths.getDataFileName("index.html") >>= \n -> putStrLn $ "DEBUG: getDataFileName(\"index.html\"): " <> n
    prov <- makeStore
    runApp flags $ MwSt.staticPolicy (py ddir) $ service env prov
    where
        py :: String -> MwSt.Policy
        py ddir = (MwSt.only [("", "index.html")] MwSt.<|> mempty) <> MwSt.addBase ddir

splitTime :: Time -> (Time -> a -> a) -> Time -> a -> a
splitTime maxdt update dt model =
    if maxdt < dt then splitTime maxdt update (dt - maxdt) (update maxdt model)
    else update dt model

data Flags = Flags
        { flagsPort :: Int       -- server port
        }

defaultFlags :: Flags
defaultFlags = Flags
    { flagsPort = 4040
    }

type FlagsProc = Flags -> Either String Flags
optionsDescr :: [OptDescr FlagsProc]
optionsDescr =
    [ Option ['p'] ["port"] (ReqArg setPort "PORT")
        "sever port (default is 4040)"
    ]
    where
        setPort :: String -> FlagsProc
        setPort arg opts =
            case readMaybe arg of
              Nothing -> Left $ "invalid argument '" ++ arg ++ "'"
              Just p -> Right (opts{ flagsPort = p })

parseArguments :: [String] -> IO Flags
parseArguments argv | elem "--help" argv = do
    prog <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ prog ++ " [OPTION...]") optionsDescr
    exitSuccess
parseArguments argv =
    case getOpt Permute optionsDescr argv of
        (fs,[],[]) ->
            case foldM (flip id) defaultFlags fs of
                Left err -> exitError $ err ++ "\n"
                Right opts -> return opts
        (_,_,errs) -> exitError $ concat errs
    where
        exitError msg = do
            prog <- getProgName
            die $ prog ++ ": " ++ msg ++
                "Try '" ++ prog ++ " --help' for more information"

runApp :: Flags -> Application -> IO ()
runApp flags app = do
#if defined(WAI_CGI)
    run                         -- CGI adapter
#elif defined(WAI_WARP)
    runEnv (flagsPort flags)    -- WARP server
#endif
        excMiddleware
    where
        excMiddleware :: Application
        excMiddleware req respond = do
            eer <- try $ app req respond
            case eer of
                Right r -> pure r
                Left exc -> -- Exception
                    respond $ errorResponse conflict409 $
                        "Exception on the CGI application: " <> tshow (exc :: SomeException)

data Env a = Env
        { eViewXMax :: !Int
        , eViewYMax :: !Int
        , eInit :: !a
        , eInput :: !(Event -> a -> a)
        , eVisual :: !(a -> Drawing)
        }

-- ---------------------------------------------------------------
-- Request processing
-- ---------------------------------------------------------------

protoVersion :: Text
protoVersion = "DRAWING/0.2"

-- Protocol description:
--  Path        Method
--  /start      POST
--  /events     POST
--  /stop       POST

data AppState a = AppState
    { appStartTime :: UTCTime   -- server start time
    , appSrvTime :: UTCTime     -- server timestamp of this saved state
    , appClientTime :: Double   -- relatime time (seconds from client start) of the last update
    , appModel :: a             -- model
    }

service :: Env a -> Connection (AppState a) -> Application
service env prov req respond = do
    let path = pathInfo req
        method = requestMethod req
    case path of
        ["start"] -> if method /= "POST"
                then respond $ errorResponse methodNotAllowed405 $ "Bad method " <> T.pack (show method)
                else postStart env prov req respond
        ["events"] -> if method /= "POST"
                then respond $ errorResponse methodNotAllowed405 $ "Bad method " <> T.pack (show method)
                else postEvents env prov req respond
        ["stop"] -> if method /= "POST"
                then respond $ errorResponse methodNotAllowed405 $ "Bad method " <> T.pack (show method)
                else postStop env prov req respond
        _ -> respond $ errorResponse notFound404 $ "Invalid path " <> T.pack (show path)

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> Maybe a
tread = readMaybe . T.unpack

postStart :: Env a -> Connection (AppState a) -> Application
postStart env conn =
    jsonHandle parseStartJson $ \ mbGuessStartTime -> do
        timestamp <- getCurrentTime
        asts <- getStateList conn
        let guessStartTime = maybe timestamp (min timestamp) mbGuessStartTime
            decission = case asts of
                (_, AppState activeStartTime activeLastTime _ _) : _ ->
                    if activeStartTime == guessStartTime then
                        -- Request is from the actual active client. Don't change the start time
                        Right activeStartTime
                    else
                        -- Request is from a client different than the actual active client
                        if timestamp > addUTCTime maxInactiveTime activeLastTime then
                            -- start                last   t
                            -- /++++++++++++++++++++++/..../     t - last > maxInactiveTime
                            -- Actual active client has no activity => abort actual active/start new client
                            Right timestamp
                        else if addUTCTime maxActiveTime activeStartTime > timestamp then
                            -- start            last t    start+mat
                            -- /+++++++++++++++++++/./...../     t - start < maxActiveTime
                            -- Actual active client has more avalaible time => continue actual active/new client must wait
                            let waitTime = diffUTCTime (addUTCTime maxActiveTime activeStartTime) timestamp
                            in Left ("Application occupied by other browser.\nPlease, wait "
                                     <> tshow waitTime <> " seconds and reload")
                        else
                            -- start        start+mat last t
                            -- /++++++++++++++++++++/++++/./     t - start >= maxActiveTime
                            -- Actual active client has elapsed its maximum time => abort actual active/start new client
                            Right timestamp
                [] ->
                    -- No actual active client => start new client
                    Right timestamp
        case decission of
            Left err -> pure $ pairs $ "version" .= protoVersion <> "error" .= err
            Right startTime -> do
                let model0 = eInit env
                deleteAllStates conn
                sessionId <- addState conn (AppState startTime timestamp 0.0 model0)
                let drw = eVisual env model0
                    ---(unitSize, svg) = renderSvgText (eViewXMax env) (eViewYMax env) drw
                pure $ pairs $ "version" .= protoVersion <> "sessionId" .= sessionId <> "startTime" .= startTime
                             <> "draw" .= drw
                             <> "viewXMax" .= eViewXMax env <> "viewYMax" .= eViewYMax env <> "unitSize" .= unitSize

parseStartJson :: Object -> Parser (Maybe UTCTime)
parseStartJson obj = do -- saved start time (serves as a client identifier)
    ---either (const Nothing) Just <$> (obj .: "startTime")
    jval <- obj .: "startTime"
    pure $ parseMaybe parseJSON jval

maxInactiveTime :: NominalDiffTime
maxInactiveTime = 5.0   -- seconds

maxActiveTime :: NominalDiffTime
maxActiveTime = 300.0   -- 5 minutes

postStop :: Env a -> Connection (AppState a) -> Application
postStop _ conn =
    jsonHandle parseStopJson $ \ sessionId -> do
        deleteState conn sessionId
        pure emptyObject_

parseStopJson :: Object -> Parser SessionId
parseStopJson obj =
    obj .: "sessionId"

postEvents :: Env a -> Connection (AppState a) -> Application
postEvents env conn =
    jsonHandle parseEventsJson $ \ (sessionId, evTime, events) -> do
        mbast <- getState conn sessionId
        case mbast of
            Nothing -> pure $ pairs $ "error" .= ("Application aborted (adquired by other browser)"::Text)
            Just (AppState startTime _ prevTime prevModel) -> do
                timestamp <- getCurrentTime
                let eventf (t1, m1) (tev, ev) =
                        let m2 = if tev > t1 then eInput env (TimePassing $ tev - t1) m1 else m1
                        in (tev, eInput env ev m2)
                    (time1, model1) = foldl eventf (prevTime, prevModel) events
                    nextModel = if evTime > time1 then eInput env (TimePassing $ evTime - time1) model1 else model1
                updateState conn sessionId (AppState startTime timestamp evTime nextModel)
                let drw = eVisual env nextModel
                    ---(unitSize, svg) = renderSvgText (eViewXMax env) (eViewYMax env) drw
                pure $ pairs $
                        "time" .= evTime <> "draw" .= drw
                        <> "viewXMax" .= eViewXMax env <> "viewYMax" .= eViewYMax env <> "unitSize" .= unitSize

parseEventsJson :: Object -> Parser (SessionId, Time, [(Time, Event)])
parseEventsJson obj = do
        sessionId <- obj .: "sessionId" -- session identifier
        time <- obj .: "time"           -- time before the first event
        jevents <- obj .: "events"
        events <- flip mapM jevents $ \ e -> do
                tm <- e .: "time"
                ev <- e .: "event"
                pure (tm, read ev)
        pure (sessionId, time, events)

-- ---------------------------------------------------------------
-- Parsing JSON request body
-- ---------------------------------------------------------------

jsonHandle :: (Object -> Parser a) -> (a -> IO Encoding) -> Application
jsonHandle fromObject action req respond = do
    ejval <- getRequestJSON req fromObject
    case ejval of
        Left err -> respond $ errorResponse badRequest400 $ "Invalid parameters: " <> err
        Right params -> do
            jenc <- action params
            respond $ jsonResponse [] $ jenc

getRequestJSON :: Request -> (Object -> Parser a) -> IO (Either Text a)
getRequestJSON req fromObject = do
    ejval <- getRequestJSON' req
    case ejval of
        Left err -> pure $ Left err
        Right jval ->
            case parseEither (withObject "Request body" fromObject) jval of
                Left err -> pure $ Left $ T.pack err
                Right v -> pure $ Right v

getRequestJSON' :: Request -> IO (Either Text Value)
getRequestJSON' req = do
    case lookup "content-type" (requestHeaders req) of
        Nothing -> pure $ Left "Request body with application/json content expected"
        Just ctype ->
            if "application/json" /= fst (B8.break (';' ==) ctype) then
                pure $ Left "Request body with application/json content expected"
            else do
                lbytes <- strictRequestBody req
                case eitherDecode lbytes of
                    Left err -> pure $ Left $ T.pack err
                    Right v -> pure $ Right v


-- ---------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------

mimeHtml :: ByteString
mimeHtml = "text/html;charset=UTF-8"

mimeSvg :: ByteString
mimeSvg = "image/svg+xml"

mimeJson :: ByteString
mimeJson = "application/json"

htmlResponse :: [Header] -> Text -> Response
htmlResponse hs html =
    let headers = ("Content-Length", B8.pack $ show $ T.length html)
                  : ("Content-Type", mimeHtml)
                  : hs
    in responseBuilder ok200 headers $ Bd.byteString $ T.encodeUtf8 html

jsonResponse :: [Header] -> Encoding -> Response
jsonResponse hs jenc =
    let bs = encodingToLazyByteString jenc
        headers = ("Content-Length", B8.pack $ show $ BL.length bs)
                  : ("Content-Type", mimeJson)
                  : hs
    in responseLBS ok200 headers bs

{---
redirectResponse :: ByteString -> Request -> Response
redirectResponse url req =
    let hs = [ ("Location", url), ("Cache-Control", "no-control") ]
    in responseBuilder seeOther303 hs mempty
---}

errorResponse :: Status -> Text -> Response
errorResponse status msg =
    let hs = [ ("Content-Type", mimeHtml) ]
        html = "<!DOCTYPE html><html><head><title>Drawing Error</title></head><body>\n"
                <> "<h2>ERROR</h2><pre><font color=\"red\">" <> escapeHtml <> "</font></pre>\n"
                <> "</body></html>\n"
        escapeHtml = msg
    in responseBuilder status hs (Bd.byteString $ T.encodeUtf8 html)

