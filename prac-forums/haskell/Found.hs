-- El mòdul Found defineix les rutes, les funcions d'autenticació i altres utilitats que s'utilitzen en l'aplicació web ForumsApp. Aquestes definicions són importants per a l'estructura i el funcionament global de l'aplicació.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Found
where
import           Config
import           Model

import           Develop.DatFw
import           Develop.DatFw.Handler
import           Develop.DatFw.Widget
import           Develop.DatFw.Template

import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.ByteString.Builder
import           Control.Monad.IO.Class   -- imports liftIO
import           Control.Monad.Trans.Maybe

-- ---------------------------------------------------------------
-- Definició dels tipus del site ForumsApp i de les corresponents rutes.

data ForumsApp = ForumsApp { forumsDb :: Connection }

-- Definició del tipus ForumsApp: ForumsApp és una estructura que representa l'aplicació web. Conté un camp forumsDb que és una connexió a la base de dades.

instance RenderRoute ForumsApp where
    data Route ForumsApp =
                  HomeR | ForumR ForumId | TopicR TopicId
                | LoginR | LogoutR | deleteForumR 

    renderRoute HomeR   = ([], [])
    renderRoute (ForumR tid) = (["forums",toPathPiece tid], [])
    renderRoute (TopicR qid) = (["topics",toPathPiece qid], [])
    renderRoute LoginR  = (["login"], [])
    renderRoute LogoutR = (["logout"], [])
    
    
    renderRoute (DeleteForum fid) = (["deleteForum",toPathPiece fid], [])
    renderRoute (DeleteTopic tid) = (["deleteTopic",toPathPiece tid], [])
    renderRoute (DeletePost pid) = (["deletePost",toPathPiece pid], [])
    renderRoute (EditForum tid) = (["editForum",toPathPiece fid], [])
    renderRoute ChangePassword = (["changePassword"], [])
    
    
    
-- Instància de RenderRoute per a ForumsApp: S'estableix una instància per a renderitzar les rutes de l'aplicació ForumsApp. Es defineixen les rutes disponibles com a constructors de dades, com ara HomeR (pàgina d'inici), ForumR (fòrum específic), TopicR (tema específic), LoginR (pàgina de login) i LogoutR (pàgina de logout).


-- Nota: Els tipus ForumId, TopicId i PostId són alias de 'Key ...' (veieu el model)

instance PathPiece (Key a) where
    toPathPiece (Key k) = showToPathPiece k
    fromPathPiece p = Key <$> readFromPathPiece p

-- Instància de PathPiece per a Key a: Key a és un tipus que representa una clau primària d'un registre de la base de dades. Es proporciona una implementació per convertir una clau a una cadena de text per a ús en una ruta i viceversa.

runDbAction :: (MonadHandler m, HandlerSite m ~ ForumsApp) => DbM a -> m a
runDbAction f = do
    conn <- getsSite forumsDb
    runDbTran conn f

-- runDbAction: És una funció que s'utilitza per executar una acció de la base de dades en el context d'un controlador (MonadHandler). Obté la connexió a la base de dades emmagatzemada a ForumsApp i executa l'acció en aquesta connexió.


-- ---------------------------------------------------------------
-- Instancia de WebApp (configuracio del lloc) per a ForumsApp.

instance WebApp ForumsApp where
    defaultLayout wdgt = do
        page <- widgetToPageContent wdgt
        mbmsg <- getMessage
        mbuser <- fmap (fmap snd) maybeAuth
        applyUrlRenderTo $(htmlTemplFile $ templatesDir <> "/default-layout.html")

--Instància de WebApp per a ForumsApp: S'estableix una instància de WebApp per a l'aplicació ForumsApp. Aquesta instància defineix el disseny predeterminat de les pàgines utilitzant una plantilla HTML específica.


-- ****************************************************************
-- Sistema d'autenticacio.
-- Aquestes funcions proporcionen funcionalitats d'autenticació per a l'aplicació web. Permeten obtenir l'ID d'autenticació de l'usuari autenticat, verificar si l'usuari està autenticat i obtenir les seves dades. També gestionen les redireccions relacionades amb l'autenticació, com redirigir l'usuari a la pàgina de login si no està autenticat. Això és útil per restringir l'accés a determinades parts de l'aplicació només als usuaris autenticats.

-- Utilitats a ser usades des dels handlers

authId_SESSION_KEY :: Text
authId_SESSION_KEY = "__AUTHID"

maybeAuthId :: (MonadHandler m) => m (Maybe UserId)
maybeAuthId = do
    mbsid <- lookupSession authId_SESSION_KEY
    return $ mbsid >>= fromPathPiece
    
-- És una funció que intenta obtenir l'identificador d'usuari autenticat a partir de la sessió. Utilitza la funció lookupSession per obtenir el valor emmagatzemat amb la clau authId_SESSION_KEY (que representa l'identificador d'autenticació) de la sessió. Retorna un Maybe UserId, que serà Just amb l'identificador d'usuari si l'usuari està autenticat, o Nothing si no hi ha cap identificador d'autenticació a la sessió.


requireAuthId :: (MonadHandler m, ForumsApp ~ HandlerSite m) => m UserId
requireAuthId = do
    mbaid <- maybeAuthId
    maybe handleNoAuthId pure mbaid

-- Aquesta funció és similar a maybeAuthId, però en cas que no s'obtingui cap ID d'autenticació, redirigeix l'usuari a la pàgina de login. Utilitza maybeAuthId per obtenir l'ID d'autenticació. Si no s'obté cap ID d'autenticació, s'utilitza handleNoAuthId per gestionar la redirecció.


handleNoAuthId :: (MonadHandler m, ForumsApp ~ HandlerSite m) => m a
handleNoAuthId = do
    setUltDestCurrent
    redirect LoginR

-- Aquesta funció es crida quan no hi ha cap ID d'autenticació disponible. Estableix la destinació d'última referència (setUltDestCurrent) perquè després de l'inici de sessió l'usuari sigui redirigit a la pàgina que inicialment intentava accedir. Redirigeix l'usuari a la pàgina de login (LoginR)


maybeAuth :: (MonadHandler m, HandlerSite m ~ ForumsApp) => m (Maybe (UserId, UserD))
maybeAuth = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    ae <- MaybeT $ runDbAction $ getUser aid
    pure (aid, ae)

-- Aquesta funció intenta obtenir les dades d'autenticació de l'usuari, és a dir, l'ID d'autenticació i la informació de l'usuari (UserId i UserD). Utilitza maybeAuthId per obtenir l'ID d'autenticació. A continuació, utilitza runDbAction per executar l'accés a la base de dades i obtenir la informació de l'usuari associada a l'ID d'autenticació. Retorna Nothing si no es pot obtenir l'ID d'autenticació o si no es troben les dades de l'usuari.

requireAuth :: (MonadHandler m, HandlerSite m ~ ForumsApp) => m (UserId, UserD)
requireAuth = do
    mbp <- maybeAuth
    maybe handleNoAuthId pure mbp
    
-- Aquesta funció és similar a maybeAuth, però en cas que no s'obtinguin les dades d'autenticació, redirigeix l'usuari a la pàgina de login. Utilitza maybeAuth per obtenir les dades d'autenticació. Si no s'obtenen les dades d'autenticació, s'utilitza handleNoAuthId per gestionar la redirecció.

