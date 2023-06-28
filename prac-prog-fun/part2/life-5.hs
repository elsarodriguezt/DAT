
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw
import qualified Data.Text as T

import Drawing
import Drawing.Vector

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        , gmPaused :: Bool
        , gmInterval :: Time    -- generation interval when not paused
        , gmElapsedTime :: Time -- elapsed time from last generation
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }
setGmPaused x g      = g{ gmPaused = x }
setGmInterval x g    = g{ gmInterval = x }
setGmElapsedTime x g = g{ gmElapsedTime = x }

data GridMode = NoGrid | LivesGrid | ViewGrid
    deriving (Show, Read)

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    , gmZoom = 1.0, gmShift = (0.0, 0.0)
    , gmPaused = True
    , gmInterval = 1.0 -- in seconds
    , gmElapsedTime = 0.0
    }

-----------------------------------------------------
-- A completar per l'estudiant

pointToPos :: Point -> Game -> Pos
pointToPos p game =
    let(gx,gy) = (1.0/gmZoom game)*^ p ^-^ gmShift game
    in (round gx,round gy)

novaQuadricula :: GridMode -> GridMode 
novaQuadricula NoGrid = LivesGrid 
novaQuadricula LivesGrid = ViewGrid 
novaQuadricula ViewGrid = NoGrid    
    
    
handleEvent :: Event -> Game -> Game 

handleEvent (KeyDown "G") game = setGmGridMode (novaQuadricula (gmGridMode game)) game -- Gestiona l'esdeveniment de pressionar la tecla "G". Quan això passa, la funció crida a novaQuadricula per obtenir el següent mode de visualització de la quadrícula, i després crida a setGmGridMode per actualitzar el mode de visualització del joc.

handleEvent (KeyDown "N") game = setGmBoard (nextGeneration (gmBoard game)) game -- Gestiona l'esdeveniment de pressionar la tecla "N". Quan això passa, la funció crida a nextGeneration per obtenir el següent estat de la quadrícula, i després truca a setGmBoard per actualitzar la quadrícula del joc.


handleEvent (MouseDown (x, y)) game = -- si l'esdeveniment és un clic del ratolí (MouseDown) i les coordenades del clic són (x, y), llavors:
                    let pos = (round x, round y) -- pos és la var que guarda la posició del clic (arrodonida a les coordenades enters més properes)
                        brd = gmBoard game -- brd és la var que guarda el tauler de joc actual
                    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game --Amb la funció "setCell" establim el valor de la cell a la posició "pos" en el tauler de joc a l'oposat del seu valor actual (vivint o morta). La funció "setGmBoard" s'utilitza per actualitzar el tauler de joc amb la cell modificada, i la funció retorna l'estat de joc actualitzat.
                    
                    
handleEvent (KeyDown "ARROWUP") game = -- Up shift
    setGmShift (gmShift game ^+^(1.0 / gmZoom game)*^(0,5)) game -- Estableix el desplaçament vertical del punt de vista del joc a un nou valor. La funció utilitza la funció "setGmShift" (pren dos arguments: gmShift i game)  per actualitzar la posició del joc. La nova posició es calcula sumant el vector (0.5) multiplicat per un factor d'escala que depèn del nivell de zoom actual del joc. El factor d'escala es calcula dividint 1 pel nivell de zoom actual i multiplicant-lo pel vector (0,5). L'operador "^+^" s'utilitza per sumar els vectors.

handleEvent (KeyDown "ARROWDOWN") game = -- Down shift
    setGmShift (gmShift game ^-^(1.0/gmZoom game)*^(0,5)) game

handleEvent (KeyDown "ARROWLEFT") game = -- Left shift
    setGmShift (gmShift game ^-^(1.0/gmZoom game)*^(5,0)) game
    
handleEvent (KeyDown "ARROWRIGHT") game = -- Right shift
    setGmShift (gmShift game ^+^ (1.0/gmZoom game)*^(5,0)) game


handleEvent (KeyDown "+") game = 
    if gmInterval game > 0.125 then setGmInterval (gmInterval game / 2) game -- Si l'interval de joc (que es guarda com a gmInterval a l'estat del joc) és més gran que 0,125 segons, la funció divideix l'interval per 2 i actualitza l'estat del joc amb el nou interval. 
    
    else game --Si l'interval ja és menor o igual a 0,125 segons, la funció no farà res i simplement retornarà l'estat del joc sense cap modificació.
    
handleEvent (KeyDown "-") game = 
    setGmInterval (gmInterval game *2) game -- La funció handleEvent es crida amb el joc actualitzat amb el nou interval de joc, que es calcula amb la multiplicació de l'interval actual pel nombre 2.

handleEvent (KeyDown " ") game = --esdeveniment pulsació de la tecla d'espai
    if gmPaused game then setGmPaused False game --Si el joc està en pausa, es crida a la funció "setGmPaused" per establir l'estat de pausa del joc a "False". Torna el nou estat de joc actualitzat. 
    else setGmPaused True game --Si el joc no està en pausa, es crida a la funció setGmPaused per establir l'estat de pausa del joc a True. Torna el nou estat de joc actualitzat.    
    
handleEvent (TimePassing dt) game = --si l'esdeveniment és la passada del temps. 
    if gmPaused game then game --Si el joc està en pausa, la funció retorna el joc sense fer cap canvi.
    else  -- Si el joc no està en pausa:
    
         if gmElapsedTime game + dt >= gmInterval game then --la funció comprova si el temps transcorregut des de l'última actualització del tauler (gmElapsedTime game) + el temps passat des del darrer esdeveniment (dt)és igual o superior a l'interval de temps del joc. Si és així, la funció calcula la següent generació del tauler i actualitza el joc.
         
               setGmElapsedTime 0 $ setGmBoard (nextGeneration (gmBoard game)) game -- La primera funció, nextGeneration, pren el taulell actual del joc (gmBoard game) i el passa a través d'una funció que calcula la següent generació del joc, retornant el nou tauler resultant. La segona funció, setGmBoard, pren aquesta nou tauler i el configura com al tauler actual del joc. Finalment, la funció setGmElapsedTime estableix el temps transcorregut en el joc a 0.
               
         else setGmElapsedTime (gmElapsedTime game + dt) game --Si no és així, la funció gmElapsedTime retorna el temps transcorregut fins ara en el joc, que s'incrementa amb dt. Després, la funció setGmElapsedTime actualitza l'estat del joc amb la nova marca de temps.

handleEvent ( KeyDown "I" ) game = -- Zoom in, comprova si el factor de zoom actual del joc, que està emmagatzemat a la variable gmZoom de game, és menor que 3.0. Si això és cert, llavors s'actualitza la var gmZoom amb el seu valor actual multiplicat per 2.0 (fent zoom in). Si el factor de zoom ja és 3.0 o superior, llavors no es fa res i es retorna l'estat actual del joc, que està representat per la variable game.
    if gmZoom game < 3.0 then setGmZoom (gmZoom game * 2.0) game
    else game -- es retorna l'estat actual del joc
    
handleEvent ( KeyDown "O" ) game = -- Zoom out, es comprova si el factor de zoom actual del joc, que està emmagatzemat a la variable gmZoom de game, és més gran que 0.3. Si això és cert, llavors s'actualitza la variable gmZoom amb el seu valor actual dividit per 2.0 (fent zoom out). Això significa que el joc es mostrarà a una escala més petita
    if gmZoom game > 0.3 then setGmZoom (gmZoom game / 2.0) game
    else game  -- es retorna l'estat actual del joc 
                        
handleEvent _ game = game

--scaledAndTranslated (x,y) zoom d = 
 --   scaled zoom zoom (translated x y d)
scaledAndTranslated (x, y) zoom d =
  translated x y (scaled zoom zoom d)
  
modeQuadricula mode x y game = 
    case mode of
         NoGrid -> blank
         LivesGrid -> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game)
         ViewGrid -> drawGrid (pointToPos((-x),(-y)) game)(pointToPos((x),(y)) game)
    
draw game = 
    let 
        gridMode = gmGridMode game
        board = gmBoard game
        zoom = gmZoom game 
        x = (viewWidth/2)
        y = (viewHeight/2)
    in drawIndex guia <> scaledAndTranslated (gmShift game) zoom (drawBoard board <> (modeQuadricula gridMode x y game)) -- Igual que anteriorment però ara concatenem el dibuix de la guia. Abans: La funció dibuixa el tauler del joc utilitzant drawBoard board, que torna una representació gràfica del tauler. La representació del tauler s'escala i es mou utilitzant la funció scaledAndTranslated, que pren el desplaçament gmShift game i el nivell de zoom zoom com a paràmetres. Després, la funció modeQuadricula gridMode x i game dibuixa una quadrícula al tauler en funció del mode de quadrícula i la posició central de la pantalla. La funció torna el resultat de la concatenació del tauler dibuixat i la quadrícula 

drawIndex index = foldMap textGuia (zip[0..] index) -- Aquest codi defineix una funció anomenada "drawIndex" que pren una llista de tuples com a entrada ("index"), i torna una "Drawing" com a resultat. La funció utilitza la funció "foldMap" per aplicar la funció "textGuia" a cada element de la llista "index". La funció "zip [0..] index" s'encarrega de crear una llista de tuples on cada element de la llista "index" està associat amb el seu índex corresponent.


textGuia :: (Int, (String, String)) -> Drawing -- pren una tupla amb un enter i una parella de cadenes de caràcters com a argument i retorna una "Drawing" que representa dos textos (les cadenes de caràcters) dibuixats en posicions diferents a la pantalla.
textGuia (i, (s1, s2)) = -- descompon la tupla d'entrada en l'enter i la llista de 2 elements de cadenes de text.
--Definim les coordenades on es dibuixaran les cadenes de text.
     let dx1 = ((-viewWidth)/2 + 1)  --situem les dues cadenes de text a diferents posicions horitzontals de la pantalla.
         dx2 = dx1 + 7
         dy = (viewHeight/2 - 1 - fromIntegral i) --situa les cadenes de text en diferents posicions verticals de la pantalla, depenent del valor de l'enter d'entrada (convertit en "fromIntegral")
         
         
     --in colored blue $ translated dx1 dy (atext startAnchor s1) <> translated dx2 dy (atext startAnchor s2) 
     
     --Error: el tipus de dades que s'espera és Data.Text.Internal.Text, però s'està proporcionant un valor de tipus String. Això pot passar quan hi ha una confusió entre String i Text al codi.

-- Solució al problema anterior: per solucionar aquest error, cal convertir la cadena String a Text. Això es pot fer usant la funció Data.Text.pack, que converteix una cadena de String a Text.

         in colored blue $ translated dx1 dy (atext startAnchor (T.pack s1)) <> translated dx2 dy (atext startAnchor (T.pack s2))  -- Creem un objecte de dibuix que consisteix en les dues cadenes s1 i s2 de text en color blau, amb les seves posicions determinades per les variables dx1, dx2 i dy. Les cadenes de text es creen amb la funció "atext" amb l'argument "startAnchor" (una altra funció) que indica l'ancoratge de la cadena de text. 


guia :: [(String, String)]
guia = [("N", "Next Step"), ("G", "Change to grid mode"), ("O", "Zoom out"), ("I", "Zoom in"), ("ARROWUP", "Shift up"), ("ARROWDOWN", "Shift down"), ("ARROWRIGHT", "Shift left"), ("ARROWLEFT", "Shift Right"), ("SPACE", "Pause/run toggle"),("+", "Increase run velocity"), ("-", "Decrease run velocity"),("Use the mouse to  set live/dead cells", "")]






