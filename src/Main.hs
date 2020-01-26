{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Web.Scotty as Scotty
import Data.IORef
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.String as String
import qualified Network.Wai.Middleware.Gzip as Scotty
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text.Encoding as TextEnc
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.HashMap.Strict as HashMap
import qualified TSP as TSP
import qualified TSPData as TSPData
import qualified Control.Monad.Parallel as Par

type Context = (SkipChan Text, IORef Bool, Text)
type ContextMap = HashMap.HashMap Text Context

data ClientCommand =
    Comm_Connect Text
    | Comm_Start Text
    | Comm_Stop Text
    | Comm_Remove Text
    | Comm_Unknown

main :: IO ()
main = do
    let port = 8083
    let settings = Warp.setPort port Warp.defaultSettings
    (session' :: IORef ContextMap) <- newIORef HashMap.empty
    sapp <- scottyApp
    Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp session') sapp

scottyApp :: IO Wai.Application
scottyApp = Scotty.scottyApp $ do

    Scotty.middleware $ Scotty.gzip $ Scotty.def { Scotty.gzipFiles = Scotty.GzipCompress }
    Scotty.middleware Wai.logStdoutDev

    Scotty.get "/" $
        Scotty.file "index.html"

-- type ServerApp = PendingConnection -> IO ()
wsapp :: IORef ContextMap -> WS.ServerApp
wsapp session' pending = do
    putText "ws connected"
    conn <- WS.acceptRequest pending

    WS.withPingThread conn 30 (return ()) $ do
        (flip finally) disconnect $ forever $ do
            -- receive our first commmand.
            putText "waiting for the client to send a command..."
            (msg :: Text) <- WS.receiveData conn
            putText $ "got command " <> msg <> "."

            -- and route it
            routeClientCommand (parseClientCommand msg) conn session'
        where
            disconnect = do
                putText "disconnected (wsapp)"

parseClientCommand :: Text -> ClientCommand
parseClientCommand msg =
    let opcode = List.head $ Text.words msg
        params = List.tail $ Text.words msg in
    oproute opcode params
    where
        oproute opcode (p1:[])
            | opcode == "$>connect" = Comm_Connect p1
            | opcode == "$>start"   = Comm_Start   p1
            | opcode == "$>stop"    = Comm_Stop    p1
            | opcode == "$>remove"  = Comm_Remove  p1
            | otherwise             = Comm_Unknown
        oproute opcode _ = Comm_Unknown

routeClientCommand :: ClientCommand -> WS.Connection -> IORef ContextMap -> IO ()
routeClientCommand (Comm_Connect to) conn session' = existingContextConnector conn to session'
routeClientCommand (Comm_Start   to) conn session' = createNewSessionAndAttachClient conn to session'
routeClientCommand (Comm_Stop    to) conn session' = do
    sessionMap <- readIORef session'
    let (session :: Maybe Context) = HashMap.lookup to sessionMap
    case session of
        Nothing -> do
            putText "No existing session to stop."
        Just (_, stopflag, _) -> writeIORef stopflag True
routeClientCommand (Comm_Unknown) conn session' = do
    WS.sendTextData conn ("#>unknown_command"::Text)

existingContextConnector :: WS.Connection -> Text -> IORef ContextMap -> IO ()
existingContextConnector conn sessionName session' = do
    sessionMap <- readIORef session'
    let (session :: Maybe Context) = HashMap.lookup sessionName sessionMap
    case session of
        Nothing -> createNewSessionAndAttachClient conn sessionName session'
        Just sess@(_, stopflag', _) -> do
            stopflag <- readIORef stopflag'
            case stopflag of
                True -> createNewSessionAndAttachClient conn sessionName session'
                False -> sessionAttach conn sess
            
createNewSessionAndAttachClient :: WS.Connection -> Text -> IORef ContextMap -> IO ()
createNewSessionAndAttachClient conn sessionName session' = do
    -- by protocol
    WS.sendTextData conn $ ("#>accepted" :: Text)

    -- this will be the waypoints file data. Set up TSP. This is sent when the
    -- "start" button is clicked on the client side.
    (wdat :: Text) <- WS.receiveData conn

    channel <- newSkipChan
    stopflag <- newIORef False
 
    putText $ "session name: " <> sessionName
    sessionMap <- readIORef session'
    writeIORef session' $
        HashMap.insert sessionName (channel, stopflag, wdat) sessionMap 

    forkIO $ producer channel stopflag wdat
    sessionAttach conn (channel, stopflag, wdat)

sessionAttach conn session@(channel, stopflag, waypointsData) = do
    WS.sendTextData conn $ "#>waypoints" <>  waypointsData

    detach' <- newIORef False
    forkIO $ whileRunListener conn session detach'

    (flip finally) (disconnect) $ foreverUntil (readIORef detach') $ do
        threadDelay $ 1 * 20000
        val <- getSkipChan channel
        WS.sendTextData conn val

    WS.sendTextData conn ("#>detached" :: Text)

    where
        disconnect = do
            putText "disconnected (sessionAttach)"

foreverUntil :: IO Bool -> IO a -> IO ()
foreverUntil condition f = condition >>= (\b -> on b)
    where on True = return ()
          on False = f >> foreverUntil condition f

whileRunListener conn (_, stopflag, _) detach' = foreverUntil (readIORef detach') $ do
    putText "whileRunListener: listening for detach..."
    (msg :: Text) <- WS.receiveData conn
    case msg of
        "$>detach" -> do
            putText "whileRunListener: got $>detach."
            writeIORef detach' True
            WS.sendTextData conn ("#>ack"::Text)
        "$>stop" -> do
            putText "whileRunListener: got $>stop."
            mapM_ (flip writeIORef True) [detach', stopflag]
            WS.sendTextData conn ("#>ack"::Text)
        _ -> do
            putText $ "whileRunListener: got unknown command " <> msg
            WS.sendTextData conn ("#>unknown_command"::Text)

producer :: SkipChan Text -> IORef Bool -> Text -> IO ()
producer channel stopflag wdat = do
    -- convert the tsp waypoints raw string data as TSPData.Graph.
    let g = TSPData.load $ Text.unpack wdat

    -- Do TSP.run, with 'printer' being the output
    TSP.run g (printer channel) stopflag

    where 
        printer :: SkipChan Text -> String.String -> IO ()
        printer channel str = putSkipChan channel $ Text.pack str

--
-- skip channel
--
data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

newSkipChan :: IO (SkipChan a)
newSkipChan = do
    sem <- newEmptyMVar
    main <- newMVar (undefined, [sem])
    return (SkipChan main sem)

putSkipChan :: SkipChan a -> a -> IO ()
putSkipChan (SkipChan main _) v = do
    (_, sems) <- takeMVar main
    putMVar main (v, [])
    mapM_ (\sem -> putMVar sem ()) sems

getSkipChan :: SkipChan a -> IO a
getSkipChan (SkipChan main sem) = do
    takeMVar sem
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return v

dupSkipChan :: SkipChan a -> IO (SkipChan a)
dupSkipChan (SkipChan main _) = do
    sem <- newEmptyMVar
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return (SkipChan main sem)

