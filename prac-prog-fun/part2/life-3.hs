
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing
import Drawing.Vector

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }

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
                    
                    
  -- Movem la figura 
handleEvent (KeyDown "ARROWUP") game = -- Up shift
    setGmShift (gmShift game ^+^(1.0 / gmZoom game)*^(0,5)) game -- Estableix el desplaçament vertical del punt de vista del joc a un nou valor. La funció utilitza la funció "setGmShift" (pren dos arguments: gmShift i game)  per actualitzar la posició del joc. La nova posició es calcula sumant el vector (0.5) multiplicat per un factor d'escala que depèn del nivell de zoom actual del joc. El factor d'escala es calcula dividint 1 pel nivell de zoom actual i multiplicant-lo pel vector (0,5). L'operador "^+^" s'utilitza per sumar els vectors.
    
setGmShift que pren dos arguments: gmShift i game
handleEvent (KeyDown "ARROWDOWN") game = -- Down shift
    setGmShift (gmShift game ^-^(1.0/gmZoom game)*^(0,5)) game

handleEvent (KeyDown "ARROWLEFT") game = -- Left shift
    setGmShift (gmShift game ^-^(1.0/gmZoom game)*^(5,0)) game
    
handleEvent (KeyDown "ARROWRIGHT") game = -- Right shift
    setGmShift (gmShift game ^+^ (1.0/gmZoom game)*^(5,0)) game
    
-- Zoom ha d'anar al final (5 Tamanys de zoom)

handleEvent ( KeyDown "I" ) game = -- Zoom in, comprova si el factor de zoom actual del joc, que està emmagatzemat a la variable gmZoom de game, és menor que 3.0. Si això és cert, llavors s'actualitza la var gmZoom amb el seu valor actual multiplicat per 2.0 (fent zoom in). Si el factor de zoom ja és 3.0 o superior, llavors no es fa res i es retorna l'estat actual del joc, que està representat per la variable game.
    if gmZoom game < 3.0 then setGmZoom (gmZoom game * 2.0) game
    else game -- es retorna l'estat actual del joc
    
handleEvent ( KeyDown "O" ) game = -- Zoom out, es comprova si el factor de zoom actual del joc, que està emmagatzemat a la variable gmZoom de game, és més gran que 0.3. Si això és cert, llavors s'actualitza la variable gmZoom amb el seu valor actual dividit per 2.0 (fent zoom out). Això significa que el joc es mostrarà a una escala més petita
    if gmZoom game > 0.3 then setGmZoom (gmZoom game / 2.0) game
    else game  -- es retorna l'estat actual del joc
                        
handleEvent _ game = game -- Altrament retorna l'estat actual del joc

--scaledAndTranslated (x,y) zoom d =  
  --  scaled zoom zoom (translated x y d)
-- Aquesta funció té tres paràmetres: (x,y), zoom, i d. La funció aplica una transformació d'escala amb el factor zoom i una transformació de translació que mou la figura d a la posició (x,y). Això retorna la figura transformada.
scaledAndTranslated (x, y) zoom d =
  translated x y (scaled zoom zoom d)

-- La función aplica dos transformaciones al objeto "d": Primero, la función "translated" desplaza el objeto "d" a las coordenadas (x,y), es decir, la tupla que se le pasó como primer argumento. Luego, la función "scaled" escala el objeto resultante de la primera transformación, utilizando el factor de escala "zoom" que se le pasó como segundo argumento.Finalmente, la función devuelve el objeto resultante de las dos transformaciones realizadas.

modeQuadricula mode x y game = --Aquesta funció té quatre paràmetres: mode, x, y, i game. 
    case mode of
         NoGrid -> blank -- Si mode és NoGrid, la funció retorna la figura blank.
         LivesGrid -> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game) -- Si mode és LivesGrid, la funció crida la funció drawGrid amb els paràmetres minLiveCell (gmBoard game) i maxLiveCell (gmBoard game).
         ViewGrid -> drawGrid (pointToPos((-x),(-y)) game)(pointToPos((x),(y)) game) -- Si mode és ViewGrid, la funció crida la funció drawGrid amb els paràmetres pointToPos((-x),(-y)) game i pointToPos((x),(y)) game. Això dibuixa una quadrícula que es mostra a la pantalla.  
-- pointToPos fa una conversió de les coordenades (x,y) de la pantalla a les coordenades del tauler de joc. Això és útil per a determinar la cel·la del tauler que es vol modificar o consultar en resposta a les interaccions de l'usuari amb la pantalla.

-- Aquesta funció té un paràmetre game. La funció defineix quatre variables locals: gridMode (mode de quadrícula del joc), board (tauler), zoom, x, i y (coordenades del punt mitjà de la vista del joc).          
draw game = 
    let 
        gridMode = gmGridMode game
        board = gmBoard game
        zoom = gmZoom game 
        x = (viewWidth/2)
        y = (viewHeight/2)  
    in scaledAndTranslated (gmShift game) zoom (drawBoard board <> modeQuadricula gridMode x y game) -- La funció dibuixa el tauler del joc utilitzant drawBoard board, que torna una representació gràfica del tauler. La representació del tauler s'escala i es mou utilitzant la funció scaledAndTranslated, que pren el desplaçament gmShift game i el nivell de zoom zoom com a paràmetres. Després, la funció modeQuadricula gridMode x i game dibuixa una quadrícula al tauler en funció del mode de quadrícula i la posició central de la pantalla. La funció torna el resultat de la concatenació del tauler dibuixat i la quadrícula


           
{--
draw game = 
        case gmGridMode game of 
                NoGrid -> transform (translation (gmShift game)) (scaled(gmZoom game) (gmZoom game) $ drawBoard (gmBoard game))
                LivesGrid -> transform (translation (gmShift game)) (scaled (gmZoom game)(gmZoom game) $ drawBoard (gmBoard game)) <> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game)
                ViewGrid -> transform (translation (gmShift game)) (drawGrid ((round (-viewWidth)), (round (-viewHeight))) ((round viewWidth), (round viewHeight))) <> scaled (gmZoom game) (gmZoom game) (drawBoard (gmBoard game))
--}










