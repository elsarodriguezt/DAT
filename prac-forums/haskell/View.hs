-- Aquest codi utilitza la llibreria "Develop.DatFw" i altres mòduls per a construir les vistes de l'aplicació de fòrums. Cada vista és renderitzada utilitzant una plantilla HTML específica i es fan crides a la base de dades per a obtenir la informació necessària per a mostrar en les pàgines web.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module View
where
import           Config
import           Found
import           Model

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template
import           Text.Blaze

import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Time
import           Data.Semigroup
import           Language.Haskell.TH.Syntax

-- ---------------------------------------------------------------
-- Utilities for the view components
-- ---------------------------------------------------------------

uidNameWidget :: UserId -> Widget ForumsApp
uidNameWidget uid = do
    uname <- maybe "???" udName <$> runDbAction (getUser uid)
    toWidget $ toMarkup uname
-- "uidNameWidget" és una funció que pren un "UserId" i genera un widget que mostra el nom de l'usuari corresponent. Utilitza la funció "runDbAction" per a obtenir el nom de l'usuari des de la base de dades.

dateWidget :: UTCTime -> Widget ForumsApp
dateWidget time = do
    zt <- liftIO $ utcToLocalZonedTime time
    let locale = defaultTimeLocale
    toWidget $ toMarkup $ T.pack $ formatTime locale "%e %b %Y, %H:%M" zt
-- "dateWidget" és una funció que pren un "UTCTime" i genera un widget que mostra la data i l'hora en un format específic. Utilitza la funció "liftIO" per a executar una acció IO per a convertir el "UTCTime" a la zona horària local i després utilitza la funció "formatTime" per a formatejar la data i l'hora.

pidPostedWidget :: PostId -> Widget ForumsApp
pidPostedWidget pid = do
    mbpost <- runDbAction $ getPost pid
    maybe "???" (dateWidget . pdPosted) mbpost
-- "pidPostedWidget" és una funció similar a "dateWidget", però pren un "PostId" i genera un widget que mostra la data i l'hora en què es va publicar el post corresponent.


-- ---------------------------------------------------------------
-- Views
-- ---------------------------------------------------------------

homeView :: Maybe (UserId, UserD) -> Widget ForumsApp -> Widget ForumsApp
homeView mbuser fformw = do
    forums <- runDbAction getForumList
    $(widgetTemplFile $ templatesDir <> "/home.html")
-- "homeView" és una vista que mostra la pàgina d'inici del fòrum. Rep com a paràmetres opcionalment un "Maybe (UserId, UserD)" que representa la informació de l'usuari autenticat i un widget "fformw". Aquesta vista obté la llista de fòrums des de la base de dades utilitzant "runDbAction" i després utilitza una plantilla HTML específica (carregada mitjançant "widgetTemplFile") per a renderitzar la pàgina d'inici.

forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp ()
forumView mbuser (fid, forum) = do
    topics <- runDbAction $ getTopicList fid
    $(widgetTemplFile $ templatesDir <> "/forum.html")
-- "forumView" és una vista que mostra la pàgina d'un fòrum específic. Rep com a paràmetres opcionalment un "Maybe (UserId, UserD)" que representa la informació de l'usuari autenticat i una tupla amb l'identificador del fòrum ("ForumId") i les dades del fòrum ("ForumD"). Aquesta vista obté la llista de temes associats al fòrum des de la base de dades i utilitza una plantilla HTML per a renderitzar la pàgina del fòrum.  
    
topicView :: Maybe (UserId, UserD) -> (TopicId, TopicD) -> Widget ForumsApp -> WidgetFor ForumsApp ()
topicView mbuser (tid, topic) rformw = do
    replies <- runDbAction $ getPostList tid
    $(widgetTemplFile $ templatesDir <> "/topic.html") 
    
-- "topicView" és una vista que mostra la pàgina d'un tema específic. Rep com a paràmetres opcionalment un "Maybe (UserId, UserD)" que representa la informació de l'usuari autenticat, una tupla amb l'identificador del tema ("TopicId") i les dades del tema ("TopicD"), i un widget "rformw". Aquesta vista obté la llista de respostes associades al tema des de la base de dades i utilitza una plantilla HTML per a renderitzar la pàgina del tema.

editForumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> WidgetFor ForumsApp ()
editForumView mbuser (fid, forum) = do
    topics <- runDbAction $ getTopicList fid
    $(widgetTemplFile $ templatesDir <> "/editforum.html") 
    
