
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }

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
    }

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game

handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game -- La sortida de la funció "nextGeneration" es passa com a argument a la funció "setGmBoard", juntament amb l'objecte del joc "game". La funció "setGmBoard" actualitza l'estat del tauler del joc amb la nova generació calculada.

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = (round x, round y) -- pos és la var que guarda la posició del clic (arrodonida a les coordenades enters més properes)
        brd = gmBoard game -- brd és la var que guarda el tauler de joc actual
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game
    -- cellIsLive pos brd: pren com a entrada una posició i un tauler de joc, i retorna un valor booleà que indica si la cel·la a la posició especificada està viva o no.
    -- not $ cellIsLive pos brd: valor contrari a l'estat actual de la cel·la
    -- setCell (not $ cellIsLive pos brd) pos brd: Aquesta funció pren l'estat actual de la cel·la (obtinguda de l'expressió anterior), la posició de la cel·la a actualitzar i el tauler de joc, i retorna un nou tauler de joc amb l'estat de la cel·la a la posició especificada actualitzat.
    -- setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game: pren el nou tauler de joc generat per la funció setCell i el joc actual, i actualitza el tauler de joc del joc amb el nou tauler generat. El resultat final és el joc amb el tauler actualitzat.

handleEvent _ game =                            -- Ignore other events
    game

-----------------------------------------------------
-- Drawing

draw game =
    drawBoard (gmBoard game)

