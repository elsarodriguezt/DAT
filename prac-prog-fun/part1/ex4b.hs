import Drawing

line :: Double -> Drawing
line angle = rotated linea (polyline [(0,0), (0,1)]) --La funció line pren un paràmetre angle que és un valor decimal que indica l'angle de rotació en graus, i retorna una línia que va des de l'origen fins a l'alçada d'una unitat.

branca :: Drawing
branca = line 0.3 <> line (-0.3) --La variable branca conté una branca d'un arbre, que està formada per dues línies rotades a diferents angles.
flor = colored yellow (solidCircle 0.1) --La variable flor conté una flor, que és un cercle groc.

arbre :: Double -> Drawing --La funció arbre pren un paràmetre n que indica la profunditat de l'arbre.
arbre 0 = flor --Si n és 0, la funció retorna una flor.
arbre n = branca <> rotated 0.3 (translated 0 1 (arbre(n-1))) <> rotated (-0.3) (translated 0 1 (arbre(n-1))) --En cas contrari, la funció crea una branca i la combina amb dues altres branques que es van rotant lleugerament i es van desplaçant cap amunt.

main :: IO ()
main = putSvg (polyline [(0,-1), (0,0)] <> arbre 8) -- La funció main crea un dibuix d'un arbre de profunditat 8. Es dibuixa primer una línia vertical (el tronc) des de la posició (0, -1) fins a (0, 0), i després s'afegeix l'arbre amb les branques a partir de la posició (0, 0).


