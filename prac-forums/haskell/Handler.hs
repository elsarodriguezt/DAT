-- Handler conté els controladors de ruta de l'aplicació de fòrums. Utilitza formularis per capturar dades introduïdes pels usuaris i les combina amb les vistes definides en el mòdul "View" per a generar contingut HTML dinàmic.

{-# LANGUAGE OverloadedStrings #-}

module Handler
where
import View
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Widget
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields
import Text.Blaze

import Data.Text as T

-- ---------------------------------------------------------------

markdownField :: Field (HandlerFor ForumsApp) Markdown
markdownField = checkMap
        (\ t -> if T.length t < minPostLen then Left "Text massa curt"
                else if T.length t > maxPostLen then Left "Text massa llarg"
                else Right (Markdown t))
        getMdText
        textareaField
        
-- Capturar l'entrada de text amb format Markdown.

---------------------------------------------------------------------

-- newForumForm: captura les dades necessàries per a crear un nou fòrum. Te un camp de text per al títol del fòrum i un camp de text amb format Markdown per a la descripció del fòrum.
newForumForm :: AForm (HandlerFor ForumsApp) NewForum
newForumForm =
    NewForum <$> freq textField (withPlaceholder "Introduiu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del fòrum" "Descripció") Nothing
-- S'utilitza la funció textField per crear el camp de text i es proporciona un missatge de plaça (placeholder) que s'afegirà al camp per indicar a l'usuari què ha d'introduir. S'utilitza la funció markdownField per crear el camp de text amb format Markdown i es proporciona un missatge de plaça (placeholder) per indicar a l'usuari què ha d'introduir.


-- newTopicForm: captura les dades per a crear un nou tema dins d'un fòrum. Te un camp de text per al títol del tema i un camp de text amb format Markdown per a la descripció del tema.    
   
newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
     NewTopic <$> freq textField (withPlaceholder "Introdueixi el títol del tòpic" "Titol") Nothing 
              <*> freq markdownField (withPlaceholder "Introdueixi la descripció del tòpic" "Descripció") Nothing
              
       
-- newReplyForm captura les dades per afegir una nova resposta a un tema. Te un únic camp
         
newReplyForm :: AForm (HandlerFor ForumsApp) Markdown
newReplyForm = freq markdownField (withPlaceholder "Introdueixi una resposta" "Resposta") Nothing

-- newReplyForm captura les noves dades d'un tema editat. Te un camp de text per al títol del tema i un camp de text amb format Markdown per a la descripció del tema. 
newEditForm :: AForm (HandlerFor ForumsApp) NewForum
newEditForm =
    NewForum <$> freq textField (withPlaceholder "Introduiu el títol del fòrum a canviar" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del fòrum a canviar" "Descripció") Nothing

-----------------------------

-- getHomeR i postHomeR, que són controladors per a les rutes HTTP associades a la pàgina principal de l'aplicació de fòrums.

getHomeR :: HandlerFor ForumsApp Html 
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth 
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm --genera un formulari fresc utilitzant la funció generateAFormPost i el formulari newForumForm. El resultat, fformw, és una representació del formulari que inclou widgets HTML per als camps de formulari i accions per a l'enviament del formulari.
    
    -- Return HTML content
    defaultLayout $ homeView mbuser fformw -- resultat és una representació HTML de la pàgina principal, que s'envia com a resposta utilitzant defaultLayout.



postHomeR :: HandlerFor ForumsApp Html 
postHomeR = do
    user <- requireAuth    
    (fformr, fformw) <- runAFormPost newForumForm --executa el formulari newForumForm mitjançant la funció runAFormPost, que processa l'enviament del formulari POST. El resultat és una tupla que conté el resultat del formulari (fformr) i una representació del formulari actualitzat (fformw).
    
    case fformr of 
        FormSuccess newtheme -> do --  Si el formulari s'ha omplert correctament:
            runDbAction $ addForum (fst user) newtheme -- executa una acció de la base de dades per afegir un fòrum nou utilitzant la funció addForum. S'utilitza l'usuari autenticat (fst user) i el tema nou (newtheme) com a paràmetres per afegir el fòrum.
            
            redirect HomeR --redirigeix l'usuari a la pàgina principal
        _ -> 
            defaultLayout $ homeView (Just user) fformw 



-- getForumR i postForumR són controladors per a les rutes HTTP associades a la visualització i l'enviament de formularis per als fòrums.

getForumR :: ForumId -> HandlerFor ForumsApp Html -- és un controlador per a la ruta GET per visualitzar un fòrum específic

getForumR fid = do
    -- Get requested forum from data-base.
    -- Short-circuit (responds immediately) with a 'Not found' status if forum don't exist
    
    forum <- runDbAction (getForum fid) >>= maybe notFound pure --obté el fòrum sol·licitat de la base de dades utilitzant la funció getForum i l'ID de fòrum (fid).    
    mbuser <- maybeAuth 
    
    -- Other processing (forms, ...)
    tformw <- generateAFormPost newTopicForm -- genera un formulari fresc utilitzant la funció generateAFormPost i el formulari newTopicForm. El resultat, tformw, és una representació del formulari que inclou widgets HTML per als camps de formulari i accions per a l'enviament del formulari.
    
    -- Return HTML content
    defaultLayout $ forumView mbuser (fid, forum) 


-----------------------------


postForumR :: ForumId -> HandlerFor ForumsApp Html -- indica que és un controlador per a la ruta POST per a l'enviament de formularis del fòrum
postForumR fid = do
    user <- requireAuth     
    forum <- runDbAction (getForum fid) >>= maybe notFound pure -- obté el fòrum utilitzant l'ID de fòrum (fid). 
    (tformr, tformw) <- runAFormPost newTopicForm -- executa el formulari newTopicForm utilitzant la funció runAFormPost. El resultat consisteix en una tupla que conté el resultat del formulari (tformr) i una representació actualitzada del formulari (tformw).
    case tformr of 
       FormSuccess newtopic -> do -- Si el formulari s'ha omplert correctament:
             now <- liftIO getCurrentTime
             runDbAction $ addTopic fid (fst user) newtopic now -- afegeix un tema nou al fòrum utilitzant la funció addTopic. S'utilitza l'ID de fòrum (tid), l'usuari autenticat (fst user) i el nou tema (newtopic) i l'hora actual (now) 
             redirect (ForumR fid) -- redirigeix l'usuari a la pàgina del fòrum (ForumR tid) 
       _ ->  -- si el formulari no s'ha omplert correctament
             defaultLayout $ forumView (Just user) (fid, tformw) -- crida la funció forumView amb els paràmetres Just user (usuari autenticat) i (tid, forum) (ID de fòrum i fòrum trobat). El resultat és una representació HTML de la pàgina de visualització del fòrum, que s'envia com a resposta utilitzant defaultLayout. Això permet que l'usuari vegi el formulari amb els errors i pugui corregir-los.


-----------------------

-- getTopicR i postTopicR son 2 controladors per a la ruta de visualització i enviament de formularis de resposta en l'aplicació de fòrum.


getTopicR :: TopicId -> HandlerFor ForumsApp Html -- gestiona la sol·licitud GET per a la visualització d'un tema específic
getTopicR tid = do
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure --obté el tema amb l'ID especificat (tid) mitjançant l'execució de l'acció de la base de dades getTopic. 
    user <- maybeAu    
    rformw <- generateAFormPost newReplyForm -- genera un formulari de resposta nou utilitzant la funció generateAFormPost i el formulari newReplyForm. El resultat és una representació actualitzada del formulari que es desa en la variable rformw.
    
    defaultLayout $ topicView user (tid, topic) rformw -- utilitza defaultLayout per generar la visualització HTML del tema utilitzant la funció topicView. Es passen els paràmetres user (usuari autenticat), (tid, topic) (ID de tema i tema) i rformw (formulari de resposta actualitzat). El resultat és una pàgina HTML que es retorna com a resposta.


postTopicR :: TopicId -> HandlerFor ForumsApp Html -- gestiona la sol·licitud POST per a l'enviament del formulari de resposta
postTopicR tid = do
    user <- requireAuth 
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure 
    (rformr, rformw) <- runAFormPost newReplyForm -- executa el formulari newReplyForm utilitzant la funció runAFormPost. El resultat consisteix en una tupla (rformr, rformw) que conté el resultat del formulari (rformr) i una representació actualitzada del formulari (rformw).
    
    case rformr of 
         FormSuccess newPost -> do -- Si el formulari s'ha enviat amb èxit i el contingut del formulari s'ha emmagatzemat en newPost:
             now <- liftIO getCurrentTime
             runDbAction $ addReply (tdForumId topic) tid (fst user) newPost now  -- executa l'acció addReply per afegir una nova resposta al tema. S'envien els paràmetres tdForumId topic (ID de fòrum del tema), tid (ID de tema), (fst user) (ID d'usuari autenticat) i newPost (contingut de la resposta) i now (hora actual a la que s'ha fet el post).
             redirect (TopicR tid) --  redirigeix l'usuari a la ruta de visualització del mateix tema (TopicR tid).
         _ -> -- Si no s'ha enviat amb èxit:
             defaultLayout $ topicView (Just user) (tid, topic) rformw -- utilitza defaultLayout per generar la visualització HTML del tema utilitzant la funció topicView. Es passen els paràmetres Just user (usuari autenticat), (tid, topic) (ID de tema i tema) i rformw (formulari de resposta actualitzat). El resultat és una pàgina HTML que es retorna com a resposta. Això permet que l'usuari vegi el formulari amb els errors i pugui corregir-los.
             

-------------- 
-- getEditForumR permet obtenir la pàgina de modificació d'un fòrum en funció de l'usuari autenticat i el rol de moderador assignat al fòrum. Si l'usuari és el moderador, es mostra el formulari d'edició del fòrum; sinó, l'usuari és redirigit a la pàgina del fòrum sense possibilitat de modificar-lo.

getEditForumR :: ForumId -> HandlerFor ForumsApp Html -- EditForumR` que és una gestió de ruta per obtenir la pàgina de modificació d'un fòrum
getEditForumR fid = do
     user <- requireAuth
     forum <- runDbAction (getForum fid) >>= maybe notFound pure --obtenim les dades del fòrum corresponents a l'identificador `fid` i les guardem a 'forum'.
     let title = fdTitle forum -- emmagatzemem el títol
         description = fdDescription forum -- emmagatzemem la descripció del forum
         moderator = fdModeratorId forum -- emmagatzemem l'id del moderador del forum
     
     case user of
        moderator -> do -- en el cas que l'usuari sigui el moderador del forum:
            (editformr, editformw) <- runAFormPost $ newEditForm (Just title) (Just description) --s'executa la funció `runAFormPost` per crear un formulari d'edició (`editformr`, `editformw`) utilitzant les dades del títol i la descripció del fòrum.
            defaultLayout $ editForumView fid editformw --Aquest formulari es presenta a la pàgina utilitzant la funció `defaultLayout` i la vista `editForumView`, passant l'identificador del fòrum (`fid`) i el formulari d'edició (`editformw`).
        _ -> 
            redirect (ForumR fid) -- redirigim l'usuari a la ruta `ForumR fid`


--postEditForumR és una gestió de ruta per processar la modificació d'un fòrum.  Permet processar la modificació d'un fòrum a partir de les dades enviades en un formulari. 
postEditForumR :: ForumId -> HandlerFor ForumsApp Html
postEditForumR fid = do
      (editformr, editformw) <- runAFormPost newForumForm -- Amb `runAFormPost` creem un formulari `newForumForm` per a l'edició del fòrum. Les dades d'aquest formulari es desen en les variables locals `editformr` i `editformw`.
      case editformr of
          FormSuccess editformw -> do  -- si s'han proporcionat dades vàlides per a l'edició del fòrums'ha enviat correctament:
              runDbAction $ newEditForm fid (nfTitle editformw) (nfDescription editformw) -- s'executa la funció `runDbAction` per a realitzar una acció de base de dades que crea una nova forma d'edició (`newEditForm`) amb l'identificador del fòrum (`fid`) i les dades del formulari d'edició (`nfTitle editformw`, `nfDescription editformw`). i 
              redirect $ ForumR fid --redirigim a l'usuari a la pàgina del forum
          _ -> -- si no s'ha enviat correctament
              defaultLayout $ editForumView fid editformw -- es presenta el formulari d'edició (`editformw`) a la pàgina utilitzant la funció `defaultLayout` i la vista `editForumView`.
         

deleteForumR :: ForumId -> HandlerFor ForumsApp Html
deleteForumR fid = do
    forum <- runDbAction (getForum fid) >>= maybe notFound pure --obtenim les dades del forum fid
    user <- requireAuth 
    let moderator = fdModeratorId forum  --guardem l'id del moderador
    case user of
        moderator -> do --si l'usuari és moderador
            runDbAction (deleteForum fid) -- eliminem el forum 
            redirect HomeR -- el redirigim a la pàgima HOme
        _ -> 
            redirect (ForumR fid) -- si no es moderador el redirigim al forum
              

deleteTopicR :: TopicId -> HandlerFor ForumsApp Html
deleteTopicR tid = do  
    user <- requireAuth  
    ((userId, _) <- user  -- guardem l'id de l'usuari
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure -- guardem el topic
    
    
    let fid = tdForumId topic -- agafem l'id del forum en el que esta el topic
        topic_owner = tdUserId topic --guardem l'id del propietari del topic
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    let moderator = fdModeratorId forum --guardem l'id del moderador del forum
          
    case userId of
      _ | user == moderator || user == topic_owner -> do -- si l'usuari es moderador del forum o es el propietari del topic pot eliminar el topic
         runDbAction (deleteTopic fid tid) -- eliminem el topic tid del forum fid
         redirect (ForumR fid) --redirigim l'usuari al Forum
      _ ->
        redirect (ForumR fid) -- si no es moderador o propietari també el redirigim fora al forum
   

deletePostR :: PostId -> HandlerFor ForumsApp Html
deletePostR pid = do
    user <- requireAuth
    (userId, _) <- user
    
    post <- runDbAction (getPost pid) >>= maybe notFound pure
    topic <- runDbAction (getTopic (pdTopicId post)) >>= maybe notFound pure
    let fid = tdForumId topic -- agafem l'id del forum en el que esta el topic
        topic_owner = tdUserId topic
        post_owner = pdUserId post
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    
    let moderator = fdModeratorId forum
    
    case userId of
      _ | user == moderator || user == topic_owner || user == post_owner -> do
         runDbAction $ deletePost fid (pdTopicId post) pid
         redirect (TopicR (pdTopicId post))
      _ ->
        redirect (TopicR (pdTopicId post))


     
     
     




























