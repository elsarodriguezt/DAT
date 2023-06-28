module Main where
import Drawing

myDrawing :: Drawing -- declara una variable anomenada myDrawing de tipus Drawing, que serà el nostre dibuix.
-- myDrawing = blank (no podem declarar varios myDrawing)

myDrawing = solidCircle 1

main :: IO ( )
main = putSvg myDrawing
