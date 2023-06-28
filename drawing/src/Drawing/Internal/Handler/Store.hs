
{-# LANGUAGE OverloadedStrings #-}

module Drawing.Internal.Handler.Store
where
import Data.Int
import qualified Data.Map as M
import Data.IORef

-- ---------------------------------------------------------------
-- Model: Types

type SessionId = Int64

type Connection a = IORef (SessionId, M.Map SessionId a)

makeStore :: IO (Connection a)
makeStore = newIORef (1, M.empty)

-- ---------------------------------------------------------------
-- Model: Data base connection

getState :: Connection a -> SessionId -> IO (Maybe a)
getState conn sid =
    (M.lookup sid . snd) <$> readIORef conn

getStateList :: Connection a -> IO [(SessionId, a)]
getStateList conn =
    (M.toList . snd) <$> readIORef conn

addState :: Connection a -> a -> IO SessionId
addState conn state = do
    (newsid, smap) <- readIORef conn
    writeIORef conn (newsid + 1, M.insert newsid state smap)
    return newsid

updateState :: Connection a -> SessionId -> a -> IO ()
updateState conn sid state = do
    (newsid, smap) <- readIORef conn
    writeIORef conn (newsid, M.insert sid state smap)

deleteState :: Connection a -> SessionId -> IO ()
deleteState conn sid = do
    (newsid, smap) <- readIORef conn
    writeIORef conn (newsid, M.delete sid smap)

deleteAllStates :: Connection a -> IO ()
deleteAllStates conn = do
    (newsid, _) <- readIORef conn
    writeIORef conn (newsid, M.empty)

