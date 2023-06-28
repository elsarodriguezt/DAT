{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (runEnv)

import Handler

import Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- ****************************************************************

main :: IO ()
main = do
    runEnv 4050 $ dispatchHandler gameApp

-- ****************************************************************
-- Controller

gameApp :: Handler HandlerResponse
gameApp = onMethod
    [ ("GET", doGet)
    , ("POST", doPost)
    ]
-- gameApp es un manejador de la aplicación web definido utilizando la biblioteca Handler. Este manejador se define utilizando la función onMethod, que especifica las acciones que se deben realizar para cada método HTTP (GET y POST). En este caso, se asigna la función doGet al método GET y la función doPost al método POST.



--doGet obté l'estat de sessió associat amb la clau "playState" i envia la representació HTML d'aquest estat com a resposta

doGet :: Handler HandlerResponse -- doGet és de tipus Handler HandlerResponse, el que significa que retorna un valor encapsulat en el monad Handler amb el tipus HandlerResponse.
doGet = do 
    mbestat <- getSession "playState" -- Estem cridant la funció getSession amb l'argument "playState". La funció getSession retorna un monad que conté un possible valor de l'estat de sessió associat amb la clau "playState". 
    estat <- case mbestat of
        Just s -> return s
        Nothing -> return startState
-- Comprovem el contingut de mbestat i prenem diferents accions en funció del seu valor. Si mbestat té un valor (Just s), llavors s'executa return s, el que significa que posem el valor s dins del monad. Si mbestat no té cap valor (Nothing), llavors s'executa return startState, el que significa que posem el valor startState dins del monad. El resultat de la clàusula case s'assigna a la variable estat.       
    respHtml $ htmlView estat -- La funció htmlView pren l'estat estat com a argument i retorna la representació relacionada amb HTML d'aquest estat. La funció respHtml pren aquesta representació en forma d'HTML i fa l'enviament de la resposta.


-- doPost obté el paràmetre de post "playText", actualitza l'estat de sessió "playState" amb el resultat de cridar la funció playText, i envia la representació HTML de l'estat actualitzat com a resposta.

doPost :: Handler HandlerResponse 
doPost = do 
    mbcaracters <- lookupPostParam "playText" -- Crida la funció lookupPostParam amb l'argument "playText". Aquesta funció retorna un monad que conté un possible valor associat amb el paràmetre de post "playText".
    mbestat <- getSession "playState" -- crida la funció getSession amb l'argument "playState". Aquesta funció retorna un monad que conté un possible valor de l'estat de sessió associat amb la clau "playState".
    
    let 
       estat = maybe startState id mbestat -- estat té el valor de mbestat si és Just, o bé el valor startState si és Nothing
       caracters = maybe "" id mbcaracters -- caracters té el valor de mbcaracters si és Just, o bé una cadena buida ("") si és Nothing
       nouEstat = playText caracters estat -- nouEstat és el resultat de cridar la funció playText amb els arguments caracters i estat.

    setSession "playState" nouEstat --crida la funció setSession amb els arguments "playState" i nouEstat. Això estableix l'estat de sessió associat amb la clau "playState" al valor nouEstat
   
    respHtml $ htmlView  nouEstat -- crida la funció respHtml amb l'argument htmlView nouEstat. La funció htmlView pren nouEstat com a argument i retorna la representació relacionada amb HTML d'aquest estat. 
       
    
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

-- htmlView es una función que genera una representación HTML del estado del juego. Utiliza la biblioteca Text.Blaze.Html5 para construir el HTML, mostrando el estado del juego y un formulario con un campo de texto y un

-- ****************************************************************
-- Model

type GameState = (Bool, Int) -- GameState es un sinónimo de tipo para representar el estado del juego, que consiste en una tupla de un valor booleano (Bool) y un entero (Int)

startState :: GameState
startState = (False, 0) --startState es una constante que define el estado inicial del juego. En este caso, se establece en (False, 0), lo que significa que el juego está inactivo (False) y tiene una puntuación de cero.


playChar :: Char -> GameState -> GameState
playChar c gameState = case c of
     '+' -> (fst gameState, snd gameState + 1)
     '-' -> (fst gameState, snd gameState - 1)
     '*' -> (not (fst gameState), snd gameState)
     '_' -> gameState

playString :: String -> GameState -> GameState
playString [] gameState = gameState
playString (c:cs) gameState = playString cs (playChar c gameState)
-- playString es una función que recibe una cadena de texto (String) y el estado del juego (GameState). Aplica la función playChar a cada carácter de la cadena, actualizando el estado del juego en cada iteración. Comienza con el estado inicial y se va modificando en función de los caracteres de la cadena.

playText :: Text -> GameState -> GameState
playText t = playString (T.unpack t)

--playText es una función que recibe un valor de texto (Text) y el estado del juego (GameState). Desempaqueta el valor de texto utilizando T.unpack para obtener una cadena de texto (String) y luego aplica playString a esa cadena para actualizar el estado del juego.
