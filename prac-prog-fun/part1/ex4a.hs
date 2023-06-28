import Drawing --importem la llibreria de dibuix

line :: Double -> Drawing --declarem una funció "line" que dibuixa una línia. Aquesta funció té un paràmetre "angle" de tipus Double.
line angle = rotated linea (polyline [(0,0), (0,1)]) --la funció "line" retorna una línia dibuixada amb el mètode "polyline" que té els punts (0,0) i (0,1), i que està girada segons el valor del paràmetre "angle".

branca :: Drawing --declarem una branca, que serà formada per dos línies
branca = line 0.3 <> line (-0.3) --la branca es forma combinant dues línies amb diferents rotacions (0.3 i -0.3)

arbre :: Double -> Drawing --declarem la funció "arbre", que dibuixa un arbre. Aquesta funció té un paràmetre "n" de tipus Double.
arbre 0 = blank --si "n" és zero, retorna un dibuix en blanc.
arbre n = branca <> rotated 0.3 (translated 0 1 (arbre(n-1))) <> rotated (-0.3) (translated 0 1 (arbre(n-1)))
-- si "n" és diferent de zero, es dibuixa una branca seguida de dues branques girades a la dreta i esquerra i desplaçades cap amunt, cada una dibuixada cridant recursivament la funció "arbre" amb "n-1".

main :: IO () --funció principal
main = putSvg (polyline [(0,-1), (0,0)] <> arbre 8) --la funció principal dibuixa una línia vertical (tronc, des de L'eix y: -1 a 0 ja que l'arbre comença a (0,0)) i l'arbre, amb 8 nivells.

