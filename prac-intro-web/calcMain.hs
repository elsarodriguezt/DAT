
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import qualified Network.Wai.Handler.Warp as W (runEnv)
import qualified Control.Exception as E

import           Handler
import           CalcApp

-- ****************************************************************

-- Inici d'execució del servidor.
-- Usa les funcions:
--      Adaptador WAI-Warp:          runEnv :: Port -> Application -> IO ()
--      Executor del monad Handler:  dispatchHandler :: Handler HandlerResponse -> Application
--      Aplicació calculadora:       calcApp :: Handler HandlerResponse
main :: IO ()
main = do
    -- try :: Exception e => IO a -> IO (Either e a)
    r <- E.try $ do
        let port = 4050
        putStrLn $ "Port: " <> show port
        W.runEnv port $ dispatchHandler calcApp
    case r of
        Right _ -> pure ()
        Left exc -> do
            -- Exception on initialization
            putStrLn "Exception on initialization (while excution of 'calcApp'): "
            putStrLn $ "    " ++ show (exc :: E.SomeException)

