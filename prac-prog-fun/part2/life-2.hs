
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
--Las dos funciones "setGmBoard" y "setGmGridMode" son funciones de actualización que toman un valor nuevo y un objeto "Game" y devuelven un nuevo objeto "Game" con el campo correspondiente actualizado al nuevo valor.
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
    }

-----------------------------------------------------
-- A completar per l'estudiant

novaQuadricula :: GridMode -> GridMode -- La funció novaQuadricula, que te un argument de tipus GridMode i torna un valor del mateix tipus. GridMode  representa els diferents modes de visualització de la quadrícula del joc

-- Definim les diferents transicions entre els modes de visualització de la quadrícula
novaQuadricula NoGrid = LivesGrid -- si el mode actual és NoGrid, la següent transició serà a LivesGrid
novaQuadricula LivesGrid = ViewGrid -- si el mode actual és LivesGrid, la següent transició serà a ViewGrid
novaQuadricula ViewGrid = NoGrid -- si el mode actual és ViewGrid, la següent transició serà a NoGrid


-- Processem els events com a life-1:

handleEvent :: Event -> Game -> Game -- handleEvent que te com arguments, un esdeveniment i un joc, i torna un nou joc. Event pot ser una tecla pressionada o un clic del ratolí. Game és l'estat actual del joc.

handleEvent (KeyDown "G") game = setGmGridMode (novaQuadricula (gmGridMode game)) game -- Gestiona l'esdeveniment de pressionar la tecla "G". Quan això passa, la funció crida a novaQuadricula per obtenir el següent mode de visualització de la quadrícula, i després crida a setGmGridMode per actualitzar el mode de visualització del joc.

handleEvent (KeyDown "N") game = setGmBoard (nextGeneration (gmBoard game)) game -- Gestiona l'esdeveniment de pressionar la tecla "N". Quan això passa, la funció crida a nextGeneration per obtenir el següent estat de la quadrícula, i després truca a setGmBoard per actualitzar la quadrícula del joc.

handleEvent (MouseDown (x, y)) game = -- si l'esdeveniment és un clic del ratolí (MouseDown) i les coordenades del clic són (x, y), llavors:
                    let pos = (round x, round y) -- pos és la var que guarda la posició del clic (arrodonida a les coordenades enters més properes)
                        brd = gmBoard game -- brd és la var que guarda el tauler de joc actual
                    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game --Amb la funció "setCell" establim el valor de la cell a la posició "pos" en el tauler de joc a l'oposat del seu valor actual (vivint o morta). La funció "setGmBoard" s'utilitza per actualitzar el tauler de joc amb la cell modificada, i la funció retorna l'estat de joc actualitzat.
                    
handleEvent _ game = game -- per qualsevol altra esdeveniment retorna l'estat de joc sense modificar-lo

--Dibuixem: 
--La imatge la construim a partir de la combinació de dues imatges, que són generades per les funcions drawBoard i case. 
--La primera imatge és el tauler del joc contingut en l'argument game. 
--La segona imatge la generem segons les 3 alternatives en el mode de quadrícula especificat pel joc.
draw game = (drawBoard (gmBoard game)) <> case (gmGridMode game) of
                    NoGrid -> blank -- imatge buida
                    LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell(gmBoard game)) --Si el mode és LivesGrid, llavors la imatge la generem cridant la funció drawGrid amb els paràmetres minLiveCell (gmBoard game) i maxLiveCell(gmBoard game), que són les coordenades de les cel·les vives més a l'esquerra i a dalt del tauler, i les coordenades de les cel·les vives més a la dreta i a baix del tauler, respectivament. La funció drawGrid crea una imatge que representa la quadrícula del joc.
                    
                    ViewGrid -> drawGrid ((round (-viewWidth)), (round (-viewHeight))) ((round viewWidth), (round viewHeight)) -- Si el mode de quadrícula és ViewGrid, lllavors la imatge la generem cridant la funció drawGrid amb els paràmetres ((round (-viewWidth)), (round (-viewHeight))) i ((round viewWidth), (round viewHeight)), que són les coordenades de les cantonades del tauler que es mostren a la pantalla. Aquestes coordenades estan basades en el valor de les variables viewWidth i viewHeight que es defineixen en el joc. Només es mostraran les cells dins dels límits especificats per les variables viewWidth i viewHeight.





