{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Web.Scotty as Scotty
import Data.IORef
import qualified Data.Text as Text
import qualified Data.String as String
import qualified Network.Wai.Middleware.Gzip as Scotty
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.WebSockets as WS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Data.ByteString
import qualified Data.Text.Encoding as TextEnc
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import qualified TSP as TSP
import qualified TSPData as TSPData

type Context = Maybe (SkipChan Text, IORef Bool)

main :: IO ()
main = do
    let port = 8787
    let settings = Warp.setPort port Warp.defaultSettings
    (session' :: IORef Context) <- newIORef Nothing
    sapp <- scottyApp
    Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions (wsapp session') sapp

scottyApp :: IO Wai.Application
scottyApp = Scotty.scottyApp $ do

    Scotty.middleware $ Scotty.gzip $ Scotty.def { Scotty.gzipFiles = Scotty.GzipCompress }
    Scotty.middleware Wai.logStdoutDev

    Scotty.get "/" $
        Scotty.file "index.html"

-- type ServerApp = PendingConnection -> IO ()
wsapp :: IORef Context -> WS.ServerApp
wsapp session' pending = do
    putText "ws connected"

    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
        -- receive our first commmand.
        putText "waiting for the client to send the first command..."
        (msg :: Text) <- WS.receiveData conn
        putText $ "got command " <> msg <> "."

        -- and route it
        routeClientCommand msg conn session'

routeClientCommand :: Text -> WS.Connection -> IORef Context -> IO ()
routeClientCommand msg conn session'
    | msg == "$>connect" = existingContextConnector conn session'
    | msg == "$>start"   = createNewSessionAndAttachClient conn session'
    | msg == "$>stop"    = return ()
    | otherwise          = putText $ "Unrecognized client ws command" <> msg
    
-- 
--  If session is Nothing, then there is nothing to connect to; tell the client
--  to create a new session.
--  Otherwise, connect to that session.
existingContextConnector :: WS.Connection -> IORef Context -> IO ()
existingContextConnector conn session' = do
    session <- readIORef session'
    case session of
        Nothing -> do
            putText "No existing session to connect to."
        Just sess -> sessionAttach conn sess
            
createNewSessionAndAttachClient conn session' = do
    -- by protocol
    WS.sendTextData conn $ ("#>accepted" :: Text)

    -- this will be the waypoints file data. Set up TSP. This is sent when the
    -- "start" button is clicked on the client side.
    (wdat :: Text) <- WS.receiveData conn

    channel <- newSkipChan
    stopflag <- newIORef False
    writeIORef session' (Just (channel, stopflag))

    forkIO $ producer channel stopflag wdat
    sessionAttach conn (channel, stopflag)

sessionAttach conn (channel, stopflag) = do
    (flip finally) (disconnect stopflag) $ do
        forever $ do
            threadDelay $ 1 * 100000
            val <- getSkipChan channel
            WS.sendTextData conn val
    where
        disconnect stopflag = do
            --writeIORef stopflag True
            putText "disconnected"

producer :: SkipChan Text -> IORef Bool -> Text -> IO ()
producer channel stopflag wdat = do
    -- convert tsp waypoints raw string data as TSPData.Graph.
    let g = TSPData.load $ Text.unpack wdat

    -- Do TSP.run, with 'printer' being the output
    TSP.run g (printer channel) stopflag

    where 
        printer :: SkipChan Text -> String.String -> IO ()
        printer channel str = putSkipChan channel $ Text.pack str




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

