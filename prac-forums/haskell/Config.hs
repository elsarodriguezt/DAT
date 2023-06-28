
{-# LANGUAGE OverloadedStrings #-}

module Config
where
import Data.Text (Text)


-- Fitxer de la base de dades (relatiu al directori des d'on s'executa l'aplicacio)
-- NOTA: Canvieu al vostre fitxer de la base de dades
forumsDbName :: Text
forumsDbName = "/home/entel/dat-2023p/prj/prac-forums/forumsdb.db" 

-- Directori de plantilles (relatiu al directori del paquet, des d'on es compila l'aplicacio)
templatesDir :: String
templatesDir = "./templates" 


