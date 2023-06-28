module Main 
where
import Drawing

--Copiem el codi de l'exercici 2 per dibuixar el semàfor
myDrawing :: Drawing

frame_1 = colored gray (solidRectangle 2.5 7.5)
frame_2 = colored black (rectangle 2.5 7.5)

lightBulb c y = colored c (translated 0 y (solidCircle 1))
trafficLight = frame_1 <> frame_2 <> lightBulb red 2.5 <> lightBulb yellow 0 <> lightBulb green (-2.5)

--Utilitzem les funcions recursives per construir una matriu 3x3
--Iterem primer les files

row :: Int -> Drawing -- declara una funció row que dibuixa una fila de semàfors. La funció pren un enter n que indica quantes vegades s'ha de repetir la fila.
row 0 = blank --cas base de la funció row, que retorna un dibuix en blanc si n=0
row n = trafficLight <> translated 3 0 (row(n-1)) --cas recursiu de la funció row, que retorna una fila de semàfors (trafficLight) combinada amb una altra fila de semàfors desplaçada 3 unitats a la dreta (translated 3 0) i repetida n-1 vegades.


-- Iterem les columnes

column :: Int -> Drawing --declara una funció column que dibuixa una columna de semàfors. La funció pren un enter n que indica quantes vegades s'ha de repetir la columna.
column 0 = blank --Si el paràmetre n és 0, retorna un dibuix en blanc (blank).
column n = row 5 <> translated 0 (-8) (column(n-1)) -- si n no és zero, la funció construeix la columna com una fila horitzontal de longitud 5 (row 5) combinada amb una altra columna amb una unitat menys (column(n-1)), que es troba translladada a la posició (0, -8) amb (translated 0 (-8)). Així, la funció fa una crida recursiva a ella mateixa per construir la columna completa.

myDrawing = translated (-3) 8 (column 3) --crea un dibuix anomenat "myDrawing" que consisteix en una columna de semàfors, on cada fila té 5 semàfors, i hi ha 3 files en total. El dibuix està desplaçat 3 unitats a l'esquerra i 8 unitats cap amunt respecte a la posició original. Aquesta transformació es fa mitjançant la funció "translated", que pren tres paràmetres: el primer és la posició en l'eix x, el segon és la posició en l'eix y, i el tercer és el dibuix que s'ha de transformar.

main :: IO ( )
main = putSvg (coordinatePlane <> myDrawing) --dibuixar sobre eix de coordenades
