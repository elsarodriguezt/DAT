module Main 
where
import Drawing

myDrawing :: Drawing --declara una variable anomenada myDrawing de tipus Drawing, que serà el nostre dibuix

frame_1 = colored gray (solidRectangle 2.5 7.5) -- rectangle gris sòlid amb una amplada de 2,5 i una alçada de 7,5.
frame_2 = colored black (rectangle 2.5 7.5) -- frame_2 rectangle negre (marc) amb una amplada de 2,5 i una alçada de 7,5.

lightBulb c y = colored c (translated 0 y (solidCircle 1)) -- La funció "solidCircle 1" crea un cercle de radi 1, i la funció "translated 0 y" desplaça el cercle a la posició (0, y). Finalment, la funció "colored c" assigna el color "c" al cercle i el dibuixa a la pantalla. Per tant, dibuixa una bombeta de llum de color "c" a la posició (0, y) a la pantalla.

trafficLight = frame_1 <> frame_2 <> lightBulb red 2.5 <> lightBulb yellow 0 <> lightBulb green (-2.5) --defineix la variable trafficLight, que és una combinació del rectangle gris, el rectangle negre i tres llums de semàfor en vermell, groc i verd. Les llums estan situades a diferents alçades verticals utilitzant la funció lightBulb.

myDrawing = trafficLight -- assignem la variable myDrawing al semàfor que acabem de crear.

main :: IO ( )
main = putSvg (coordinatePlane <> myDrawing) --dibuixem sobre eix de coordenades
