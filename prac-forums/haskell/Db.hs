
{-# LANGUAGE OverloadedStrings  #-}

-- Db proporciona una capa d'accés a la base de dades utilitzant SQLite per a l'aplicació Haskell. Proporciona funcions per connectar-se, tancar la connexió i realitzar operacions comunes de lectura, escriptura i eliminació d'entitats a la base de dades. Aquesta capa ajuda a simplificar les interaccions amb la base de dades i facilita la gestió de les entitats en l'aplicació.
module Db
    ( Connection, openDb, closeDb
    , runDbTran, DbM
    , Key(..), DbEntity(..)
    , get, getJust, select, selectOrder, add, set, update, delete
    , FromField(..), ToField(..), FromRow(..), ToRow(..), field
    )
where
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import Data.Text as T
import qualified Data.List as L
import Data.Int
import Data.Semigroup
import System.Directory  -- doesFileExist
import System.IO.Error  -- mkIOError, ...
import Control.Monad.Reader
import Control.Monad.IO.Class   -- imports liftIO

-- ---------------------------------------------------------------
-- Key / DbEntity
-- Key a i DbEntity a: Aquestes són dues classes de tipus utilitzades per a la identificació i la manipulació de les entitats de la base de dades. La classe DbEntity defineix mètodes per a obtenir informació sobre la taula relacionada amb l'entitat, com ara el nom de la taula, el nom de la clau principal i els noms de les columnes. La classe Key defineix un tipus Key a que representa una clau primària per a l'entitat a.

newtype Key a = Key { getKey :: Int64 }
    deriving (Eq, Show, Read)

class (FromRow a, ToRow a) => DbEntity a where
    tableName :: proxy a -> Text
    keyName :: proxy a -> Text
    columnNames :: proxy a -> [Text]
    -- Default definitions:
    keyName _ = "id"

instance FromField (Key a) where
    fromField f = Key <$> fromField f

instance ToField (Key a) where
    toField (Key x) = toField x

-- ---------------------------------------------------------------
-- Data base connection
--openDb i closeDb: s'encarreguen de connectar-se i tancar la connexió amb la base de dades SQLite. La funció openDb rep una ruta de fitxer com a argument i verifica si el fitxer de base de dades existeix. Si existeix, obre una connexió a la base de dades i activa les claus estrangeres amb la sentència SQL "PRAGMA foreign_keys=ON;". La funció closeDb tanca la connexió amb la base de dades.

openDb :: Text -> IO Connection
openDb patht = do
    let path = unpack patht
    ok <- doesFileExist path
    if ok then do
        conn <- open path
        execute_ conn "PRAGMA foreign_keys=ON;"
        pure conn
    else
        ioError $ mkIOError doesNotExistErrorType "Cannot open data base file" Nothing (Just path)

closeDb :: Connection -> IO ()
closeDb conn =
    close conn

-- ---------------------------------------------------------------
-- Data base access
-- DbM: Aquest és un sinònim de tipus que representa una mònada de transformador de mònada de lector amb Connection com a mònada interna. Aquesta mònada s'utilitza per encapsular les operacions d'accés a la base de dades.

type DbM = ReaderT Connection IO

runDbTran :: MonadIO m => Connection -> DbM a -> m a
runDbTran conn dbm = do
    liftIO $ runReaderT dbm conn

-- Les funcions tshow i interCommas són funcions auxiliars que pertanyen al mòdul Db i s'utilitzen per a operacions específiques relacionades amb la manipulació de text.

tshow :: Text -> Text
tshow name = pack $ show name
-- tshow :: Text -> Text: Aquesta funció pren un valor de tipus Text i el converteix en una cadena de text (Text). Utilitza la funció show per convertir el valor en una cadena de caràcters Haskell i després utilitza pack per convertir aquesta cadena de caràcters en Text.

interCommas :: [Text] -> Text
interCommas names = T.intercalate "," names
-- interCommas :: [Text] -> Text: Aquesta funció pren una llista de valors de tipus Text i els concatena en una sola cadena de text, separant-los amb comes (,). Utilitza la funció T.intercalate de la llibreria Data.Text per realitzar aquesta operació.


-- Funcions d'accés a la base de dades: Aquest codi proporciona diverses funcions per a realitzar operacions d'accés a la base de dades. Aquestes funcions s'utilitzen per interactuar amb la base de dades SQLite utilitzant sentències SQL.

get :: DbEntity a => Key a -> DbM (Maybe a)
get k = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "SELECT " <> interCommas colNames <> " FROM " <> tabName <> " WHERE " <> kName <> " = ?"
    rows <- query conn q (Only k)
    case rows of
        [] -> pure Nothing
        [row] -> pure $ Just row

-- get permet obtenir una entitat específica de la base de dades utilitzant la seva clau primària. Rep una clau (Key a) com a argument i retorna una acció de la mònada de transformador de mònada de lector que proporciona una possible entitat (Maybe a) com a resultat. La funció executa una consulta SQL per seleccionar les columnes corresponents a l'entitat identificada per la clau a la taula de la base de dades. Si s'ha trobat una entitat, es retorna com a Just a, si no, es retorna Nothing.


getJust :: DbEntity a => Key a -> DbM a
getJust k =
    get k >>= maybe (fail $ "Invalid foreign key " <> show k) pure

--getJust és similar a get, però en lloc de retornar Maybe a, retorna directament l'entitat a. Si no es troba cap entitat amb la clau especificada, es produeix un error mitjançant la funció fail.


select :: DbEntity a => (Key a -> a -> Bool) -> DbM [(Key a, a)]
select fwhere = ReaderT $ \ conn -> do
    let proxyForFun :: (Key a -> a -> Bool) -> Maybe a
        proxyForFun f = Nothing
        proxy = proxyForFun fwhere
        tabName = tshow $ tableName proxy
        kName = tshow $ keyName proxy
        colNames = tshow <$> columnNames proxy
        q = Query $ "SELECT " <> interCommas (kName : colNames) <> " FROM " <> tabName
        unrow (Only k :. entity) = (k, entity)
    rows <- query_ conn q
    pure $ L.filter (uncurry fwhere) $ unrow <$> rows
    
-- select permet seleccionar un conjunt d'entitats de la base de dades que compleixin una condició especificada pel predicat fwhere. Rep una funció de predicat que pren una clau i una entitat com a arguments i retorna un booleà indicant si l'entitat compleix la condició. Retorna una acció de la mònada de transformador de mònada de lector que proporciona una llista de parells (Key a, a) amb les claus i les entitats que compleixen el predicat. La funció executa una consulta SQL per seleccionar les columnes corresponents a les entitats de la taula de la base de dades i les filtra utilitzant el predicat fwhere.


selectOrder :: (DbEntity a, Ord fld) => (Key a -> a -> Bool) -> (a -> fld) -> DbM [(Key a, a)]
selectOrder fwhere forderby =
    L.sortOn (forderby . snd) <$> select fwhere
    
-- selectOrder és similar a select, però afegeix la capacitat d'ordenar les entitats en funció d'un camp especificat pel paràmetre forderby. Rep el mateix predicat fwhere i una funció que pren una entitat i retorna un camp (fld) pel qual es realitzarà l'ordenació. Retorna una llista ordenada de parells (Key a, a) amb les claus i les entitats que compleixen el predicat i estan ordenades segons el camp especificat.


add :: DbEntity a => a -> DbM (Key a)
add value = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName $ Just value
        colNames = tshow <$> (columnNames $ Just value)
        q = Query $ "INSERT INTO " <> tabName <> " (" <> interCommas colNames <> ") VALUES (" <> interCommas (const "?" <$> colNames) <> ")"
    execute conn q value
    Key <$> lastInsertRowId conn

--add permet afegir una nova entitat a la base de dades. Rep una entitat (a) com a argument i retorna una acció de la mònada de transformador de mònada de lector que proporciona la clau primària (Key a) assignada a l'entitat afegida. La funció executa una sentència SQL d'inserció per afegir l'entitat a la taula de la base de dades.

set :: DbEntity a => Key a -> a -> DbM ()
set k value = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "UPDATE " <> tabName <> " SET " <> interCommas ((<> "=?") <$> colNames) <> " WHERE " <> kName <> " = ?"
    execute conn q (value :. Only k)

--set permet actualitzar una entitat existent a la base de dades. Rep una clau primària (Key a) i una nova entitat (a) com a arguments i retorna una acció de la mònada de transformador de mònada de lector sense cap resultat (()). La funció executa una sentència SQL d'actualització per modificar les columnes corresponents a l'entitat identificada per la clau a la taula de la base de dades.


update :: DbEntity a => Key a -> (a -> a) -> DbM ()
update k f = do
    mbvalue <- get k
    case mbvalue of
        Just value -> set k (f value)
        Nothing -> pure ()
        
-- update permet actualitzar una entitat existent a la base de dades mitjançant una funció de transformació (f) que s'aplica a l'entitat actual. Rep una clau primària (Key a) i una funció de transformació (a -> a) com a arguments i retorna una acció de la mònada de transformador de mònada de lector sense cap resultat (()). La funció primer obté l'entitat actual mitjançant la funció get, després aplica la funció de transformació i finalment utilitza la funció set per actualitzar l'entitat a la base de dades.


delete :: DbEntity a => Key a -> DbM ()
delete k = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "DELETE FROM " <> tabName <> " WHERE " <> kName <> " = ?"
    execute conn q (Only k)

--delete permet eliminar una entitat de la base de dades utilitzant la seva clau primària. Rep una clau primària (Key a) com a argument i retorna una acció de la mònada de transformador de mònada de lector sense cap resultat (()). La funció executa una sentència SQL de supressió per eliminar l'entitat identificada per la clau de la taula de la base de dades.
