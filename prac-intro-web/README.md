# Web Application Interface
En Haskell es disposa d'un API bàsic per a la programació d'aplicacions WEB que és independent del servidor. Aquest API s'anomena Web Application Interface (WAI) i serà el suport per a frameworks més elaborats que facilitin el desenvolupament d'aplicacions complexes.

Com a primer exemple, veiem una simple aplicació en que es mostra un formulari a l'usuari per introduir el seu nom, i guarda durant la sessió (a través d'una cookie) el nom i el mostra en les successives interaccions.
El codi d'aquest exemple (hello-1.hs) usa el WAI i a més usa també un paquet de generació d'HTML.

Per executar l'aplicació caldrà fer:

~/dat-2023p$ cd prj/prac-intro-web

~/dat-2023p/prj/prac-intro-web$ stack build

~/dat-2023p/prj/prac-intro-web$ stack runghc nomxFitxer.hs

on nomFitxer.hs és el nom del fitxer corresponent al mòdul Main de l'aplicació.

# Realització d'un monad Handler
L'objectiu d'aquest apartat és la realització d'un monad (que anomenarem Handler) que faciliti la programació d'aplicacions WEB.
Un exemple d'us d'aquest monad el teniu en una segona versió de l'aplicació hello anterior (hello-2.hs).

# Codi
El codi de l'aplicació conté els següents mòduls Haskell:

Main:
És pròpiament l'aplicació. Conté el punt d'entrada del servidor, el codi corresponent al processat de les peticions i la generació d'HTML. (Observació: El fitxer hello-2.hs conté el mòdul Main i és l'únic en que el nom del fitxer i del mòdul poden ser diferents).

Handler:
Defineix un monad Handler, usat pel mòdul anterior, que simplifica l'aplicació.

A més s'usen altres paquets preinstal.lats de la plataforma Haskell. Alguns dels paquets usats en aquesta aplicació, específics per a la programació de la WEB, són:

  blaze-html:
  Defineix tipus, funcions i operadors per a la la generació d'HTML.
  
  wai:
  Definició del WAI. Defineix tipus i funcions per al processat de peticions i contrucció de respostes HTTP.
  La documentació de tots els paquests la podeu trobar en https://hackage.haskell.org/, que és la base de dades centralitzada dels paquets disponibles de Haskell de codi obert. També podeu usar el buscador de funcions https://hoogle.haskell.org/

# Realització d'un cas d'ús del monad Handler (Joc de la vida, fitxer: part2.hs)
En aquest apartat es demana la realització d'una senzilla aplicació WEB que usa el monad Handler que heu implementat anteriorment.

L'aplicació consisteix en un joc que processa una cadena de caràcters i produeix una puntuació (número enter) a partir de la cadena. Durant el processat el monad Handler manté un estat amb la puntuació actual i un flag que indica si el joc està activat o desactivat. El processat dels caràcters consisteix en:

- '*' commuta el joc activat o desactivat
  
- '+' incrementa la puntuació si el el joc està activat
  
- '-' decrementa la puntuació si el el joc està activat
  
- Altres caràcters s'ignoren (no canvien el joc).

La pàgina HTML de l'aplicació mostra l'estat actual del joc i un formulari que permeti a l'usuari introduir una cadena formada per caràcters com els anteriors. Inicialment el joc està desactivat i la puntuació és 0.

En cada interacció es modifica l'estat adeqüadament i es redirigeix novament a la pàgina de l'aplicació.
