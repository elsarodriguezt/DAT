import Drawing
-- Codi semafor:
frame_1 = colored gray (solidRectangle 2.5 7.5) 
frame_2 = colored black (rectangle 2.5 7.5) 
lightBulb c y = colored c (translated 0 y (solidCircle 1)) 
semafor = frame_1 <> frame_2 <> lightBulb red 2.5 <> lightBulb yellow 0 <> lightBulb green (-2.5)

trafficLight :: (Double, Double) -> Drawing -- defineix la funció "trafficLight", que pren una tupla amb dues components de tipus Double com a argument. Les components de la tupla representen les coordenades (fila, columna) del semàfor.
trafficLight (r, c) = translated c r semafor--implementa la funció "trafficLight". El semàfor és traslladat a la posició desitjada a la pantalla a través de la funció "translated". La posició és calculada a partir de les coordenades (fila, columna) passades com a argument.

list = [(8,-3), (8,0), (8,3), (0,-3), (0,0), (0,3), (-8,-3), (-8,0), (-8,3), (-8,6), (0,6), (8,6), (8,-6), (0,-6), (-8,-6)]
myDrawing = foldMap trafficLight list --defineix la variable "myDrawing", que és la combinació de tots els semàfors que es volen dibuixar a la pantalla. Es fa servir la funció "foldMap", que aplica la funció "trafficLight" a cada element de la llista "list" i després combina tots els resultats en una sola figura. En altres paraules, "myDrawing" és una representació gràfica de tots els semàfors que es volen dibuixar a les posicions especificades a la llista "list".

main :: IO()
main = putSvg (myDrawing)
