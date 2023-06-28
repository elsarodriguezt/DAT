
{-# LANGUAGE OverloadedStrings #-}

-- App defineix una aplicació web utilitzant el framework DatFw, especificant diverses rutes i els controladors associats a cada ruta. Cada ruta té diferents mètodes HTTP associats (GET, POST) i s'executaran les funcions de controlador corresponents depenent del mètode i la ruta rebuda en les sol·licituds.

module App
where
import Config
import Found
import Model
import Handler
import AuthHandler

import Develop.DatFw
import Develop.DatFw.Dispatch
--Aquests importacions són específiques del framework DatFw i proporcionen funcionalitats per a l'encaminament de sol·licituds i altres característiques de desenvolupament.
import Network.Wai
-- Aquesta importació és per al mòdul que proporciona una interfície comuna per a les aplicacions web WAI (Web Application Interface).


-- ---------------------------------------------------------------
-- Application initialization

-- Aquesta és una funció que retorna una aplicació WAI. Utilitza la funció openDb per obrir la base de dades i crear una aplicació ForumsApp que conté la base de dades com a estat.

makeApp :: IO Application
makeApp = do
    -- Open the database (the model state)
    db <- openDb forumsDbName
    toApp ForumsApp{ forumsDb = db }

-- ---------------------------------------------------------------
-- Main controller

--Aquesta instància de la classe Dispatch defineix com s'ha de despatxar cada sol·licitud que arriba a l'aplicació. Es defineixen diverses rutes amb diferents mètodes HTTP (GET, POST) i es vinculen a funcions de controlador específiques.

instance Dispatch ForumsApp where
    dispatch = routing
    
    -- La funció dispatch utilitza la funció routing per agrupar diverses rutes en un patró conjunt. Aquest patró de despatxament permet que l'aplicació respongui a diferents rutes i mètodes HTTP amb diferents funcions de controlador.
    
            $ route ( onStatic [] ) HomeR 
                [ onMethod "GET" getHomeR
                , onMethod "POST" postHomeR
                ]
                
    -- Les rutes estan definides amb la funció route i especificades amb els constructors com onStatic, onDynamic i onMethod. Per exemple, onStatic [] coincideix amb la ruta inicial (HomeR) sense cap segment addicional a l'URL.
    -- L'ús de <||> entre les rutes permet agrupar múltiples rutes en un patró conjunt. Això significa que l'aplicació intentarà coincidir amb cada ruta en l'ordre especificat fins que trobi una coincidència.
    
            <||> route ( onStatic ["forums"] <&&> onDynamic ) ForumR
                [ onMethod1 "GET" getForumR
                , onMethod1 "POST" postForumR
                ]
                
     -- Cada ruta té associades diferents funcions de controlador que es criden depenent del mètode HTTP utilitzat en la sol·licitud. Per exemple, onMethod1 "GET" getForumR especifica que la funció getForumR s'executarà només quan es rebi una sol·licitud GET per a la ruta del fòrum.
            <||> route ( onStatic ["topics"] <&&> onDynamic ) TopicR
                [ onMethod1 "GET" getTopicR
                , onMethod1 "POST" postTopicR
                ]
            <||> route ( onStatic ["login"] ) LoginR
                [ onMethod "GET" getLoginR
                , onMethod "POST" postLoginR
                ]
            <||> route ( onStatic ["logout"] ) LogoutR
                (onAnyMethod handleLogoutR)
                
     -- La ruta LogoutR té una funció de controlador handleLogoutR que s'executarà per a qualsevol mètode HTTP utilitzat en la sol·licitud.
     
     
            <||> route ( onStatic ["deleteForum"] <&&> onDynamic ) DeleteForum
                 [onMethod1 "GET" deleteForumR]
                 
            <||> route ( onStatic ["deleteTopic"] <&&> onDynamic ) DeleteTopic
                 [onMethod1 "GET" deleteTopicR]
            
            <||> route ( onStatic ["deletePost"] <&&> onDynamic ) DeletePost
                 [onMethod1 "GET" deletePostR] 
                 
            <||> route ( onStatic ["editForum"] <&&> onDynamic ) EditForum
                [ onMethod "GET" getEditForumR
                , onMethod "POST" postEditForumR
                ]
            <||> route ( onStatic ["changePassword"] <&&> onDynamic ) ChangePassword
                [ onMethod "GET" getChangePasswordR
                , onMethod "POST" postChangePasswordR
                ]
                          

