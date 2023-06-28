## Pràctica3: Aplicació WEB clàssica. Realització d'un simple forum
#Objectius
L'objectiu d'aquesta pràctica és el de realitzar una aplicació WEB clàsica (en el costat servidor) usant un framework que facilita el desenvolupament d'aquest tipus d'aplicacions.
En aquest treball es tractaran els següents aspectes:
- Disseny basat en una arquitectura Model-View-Controller (MVC).
- Reutilització de classes i patrons proporcionats per un framework (DatFw).
- Manteniment de l'estat de les sessions amb els clients.
- Autentificació d'usuaris i funcionalitat depenent de l'usuari.
- Accés a bases de dades per al manteniment persistent de la informació.
- Generació dinàmica del HTML. Definició dels components de visualització amb el llenguatge de plantilles de DatFw.

El treball consistirà en la realització d'una aplicació WEB interactiva que permeti la creació de diferents temes de discussió (forums), i la creació de qüestions (topics) i respostes en els diferents forums, pels usuaris registrats.

Funcionalitat de l'aplicació
L'aplicació a realitzar ha de permetre als usuaris les següents funcionalitats:
- Consultar la llista de fòrums creats sobre els que es poden fer qüestions i respostes. Cada fòrum ha de tenir els següets atributs:
  1. El títol (text d'una sola línia)
  2. L'usuari que ha creat el fòrum (moderador del fòrum)
  3. la descripció del fòrum (text de múltiples línies amb sintaxis markdown)
  4. La data de creació del fòrum.
- Veure les qüestions (topics) i respostes (posts) realitzades en un fòrum determinat. Cada qüestió pot tenir vàries respostes associades. Cada qüestió o topic ha de tenir els següets atributs:
  1. L'usuari que ha fet la qüestió,
  2. La data (dia i hora) en que s'ha fet la qüestió,
  3. L'assumpte de la qüestió (text d'una sola línia)
  4. El contingut de la qüestió (text markdown de múltiples línies).
Cada resposta o post ha de tenir els següents atributs:
  1. L'usuari que ha fet la resposta,
  2. La data (dia i hora) en que s'ha fet la resposta
  3. El contingut de la resposta (text markdown de múltiples línies).
- Si l'usuari és un usuari registrat i s'ha autentificat, afegir nous fòrums. En aquest cas, s'ha d'obrir un formulari que permeti introduir el títol i la descripció.
- Si l'usuari s'ha autentificat, afegir noves qüestions a un determinat fòrum. En aquest cas, s'ha d'obrir un formulari que permeti introduir l'assumpte i el contingut de la qüestió.
- Si l'usuari s'ha autentificat, afegir noves respostes a una determinada qüestió. En aquest cas, s'ha d'obrir un formulari que permeti introduir el contingut de la resposta.
- Si l'usuari s'ha autentificat i és el moderador d'un determinat fòrum, modificar el fòrum. En aquest cas, s'ha d'obrir un formulari que permeti editar el nou títol i/o la nova descripció. El moderador d'un fòrum també ha de poder eliminar les qüestions o respostes que cregui que són inadeqüades per al fòrum. Si elimina una qüestió, automàticament quedaràn eliminades totes les seves respostes.
- Si l'usuari s'ha autentificat, canviar el seu password. En aquest cas, s'ha d'obrir un formulari que permeti introduir el password actual, el nou password i una confirmació del nou password.


