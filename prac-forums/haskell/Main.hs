-- El mòdul "Main" és responsable de construir i executar l'aplicació de fòrums utilitzant la llibreria WAI i el servidor web Warp. També maneja errors durant la inicialització de l'aplicació i gestiona els arguments de la línia de comandes per a especificar el port de servidor.

{-# LANGUAGE OverloadedStrings #-}

module Main
where
import App

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Control.Exception
import System.Environment

import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

-- ****************************************************************

main :: IO ()
main = do
    -- La funcio 'makeApp' (definida en el modul App) construeix una aplicacio WAI
    -- a partir d'una aplicacio de tipus Forum (instancia de WebApp de DatFw)
    r <- try makeApp 
-- La funció intenta construir l'aplicació WAI cridant "makeApp" (definida en el mòdul "App"). Utilitza la funció "try" per a capturar excepcions que poden ser llançades durant la construcció de l'aplicació.

    case r of
        Right app -> do
            -- Warp adapter
            args <- getArgs
    -- Si la construcció de l'aplicació té èxit, s'obté una aplicació WAI i es procedeix amb la seva execució utilitzant el servidor web Warp.
            
            case listToMaybe args >>= readMaybe of
                Just port -> do
                    putStrLn $ "HTTP port is " <> show port
                    run port app
                Nothing -> do
                    prog <- getProgName
                    putStrLn $ "Usage: " <> prog <> " PORT"
                    
     -- Es comprova si s'ha proporcionat un port com a argument de la línia de comandes. Si és així, s'utilitza aquest port per a executar el servidor. En cas contrari, es mostra un missatge d'ús amb el nom del programa i el paràmetre esperat (PORT).

        Left exc -> do
            -- Exception on initialization
            putStrLn "Exception on initialization (while excution of 'makeApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

     -- Si hi ha una excepció durant la construcció de l'aplicació, s'imprimeix un missatge d'error amb la informació de l'excepció llançada.
