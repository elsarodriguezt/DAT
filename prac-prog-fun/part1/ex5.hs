import Drawing 

-- copiem el codi del semàfor:
frame_1 = colored gray (solidRectangle 2.5 7.5) 
frame_2 = colored black (rectangle 2.5 7.5) 
lightBulb c y = colored c (translated 0 y (solidCircle 1)) 
trafficLight = frame_1 <> frame_2 <> lightBulb red 2.5 <> lightBulb yellow 0 <> lightBulb green (-2.5)

repeatDraw :: (Int -> Drawing) -> Int -> Drawing -- "thing", que és una funció que rep un enter i retorna un dibuix, i "n", que és un enter que indica quantes vegades s'ha de repetir la funció "thing".
repeatDraw thing 0 = blank
repeatDraw thing n = thing n <> (repeatDraw thing (n-1)) --la funció retorna la combinació de dos elements: el dibuix que retorna la funció "thing" amb el paràmetre "n", i la crida a la mateixa funció "repeatDraw" però amb la mateixa funció "thing" i amb el paràmetre "n-1".

myDrawing = repeatDraw lightRow 3 --"myDrawing" serà el dibuix que s'obté en repetir tres vegades la funció "lightRow" i combinar els dibuixos resultants. La funció "lightRow" és una funció que rep un enter "r" i retorna un dibuix compost per cinc semàfors (com indica la seva definició), i cada semàfor es posiciona a una determinada coordenada en funció de "r" i "c"

lightRow :: Int -> Drawing 
lightRow r = repeatDraw (llum r) 5 --iteració columnes fins n-1 amb n=5, defineix la funció lightRow que rep un enter r i retorna la combinació de cinc semàfors amb la funció llum r.

llum :: Int -> Int -> Drawing --La funció pren dos arguments enters "r" i "c", que representen les coordenades de fila i columna de la llum del semàfor.
llum r c = translated (3*fromIntegral c -9)(8*fromIntegral r -16) trafficLight -- separació entre columnes, esq/dreta, separació entre files, adalt/abaix
--La funció retorna un objecte de dibuix anomenat "trafficLight", el qual es mou a una posició específica a la pantalla amb la funció "translated". En aquest cas, la posició és calculada a partir de les coordenades "r" i "c". Les coordenades són convertides a nombres de punt flotant amb "fromIntegral", i després són ajustades perquè la llum aparegui a la posició desitjada a la pantalla.

main :: IO()
main = putSvg (myDrawing)



