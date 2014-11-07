{-# LANGUAGE DeriveGeneric #-}
module Main where

import Graphics.Gloss.Interface.IO.Game

import Network.Simple.TCP (
    withSocketsDo,
    serve,HostPreference(HostAny),HostName,
    connect,
    Socket,SockAddr)

import Pipes.Network.TCP (toSocket,fromSocket)

import Pipes.Concurrent

import Pipes

import qualified Pipes.Prelude as Pipes (map)

import Data.Binary (Binary,encode,decode)
import Data.ByteString.Lazy (toStrict,fromStrict)

import Control.Concurrent.STM (
    newTVarIO,TVar,readTVarIO,readTVar,writeTVar,modifyTVar)

import Data.Set (Set,empty,singleton,union,insert,toList)
import Data.Maybe (fromJust)

import GHC.Generics (Generic)

import Control.Monad (forever,unless,forM_)
import Data.Monoid (mconcat)

data Command = Command Time PlayerName Action
    deriving (Show,Read,Eq,Ord,Generic)

instance Binary Command

type Time = Integer

type PlayerName = String

data Action = East | North | West | South | Connect
    deriving (Show,Read,Eq,Ord,Generic)

instance Binary Action

main :: IO ()
main = withSocketsDo (do

    let address = "127.0.0.1"

    (inboxO,inboxI) <- spawn Single
    (outboxO,outboxI) <- spawn Single
    commandV <- newTVarIO empty
    outputsV <- newTVarIO []

    forkIO (updateCommands inboxI outboxO commandV)

    forkIO (serveOutputs outboxI outputsV)

    peers <- getPeers address

    peerV <- newTVarIO peers

    forM_ (toList peers) (\peer -> connect peer "3894" (establishConnection peerV outputsV inboxO))

    serve HostAny "3894" (establishConnection peerV outputsV inboxO)

    playIO
        (InWindow "Kosoban" (600,600) (100,200))
        white
        20
        ()
        (render commandV)
        (handle inboxO)
        (const return))

render :: TVar (Set Command) -> () -> IO Picture
render commandsVar () = do
    commands <- readTVarIO commandsVar
    return (circle 20)

handle :: Output (Set Command) -> Event -> () -> IO ()
handle inboxO event () = do
    let command = undefined
    atomically (send inboxO (singleton command))
    return ()


updateCommands :: Input (Set Command) -> Output (Set Command) -> TVar (Set Command) -> IO ()
updateCommands inboxI outboxO commandV = forever (atomically (do
    Just incomingCommands <- recv inboxI
    currentCommands <- readTVar commandV
    let newCommands = union incomingCommands currentCommands
    unless (newCommands == currentCommands) (do
	writeTVar commandV newCommands
	send outboxO newCommands
	return ())))


serveOutputs :: Input (Set Command) -> TVar [Output (Set Command)] -> IO ()
serveOutputs outboxI outputV = forever (do
    Just commands <- atomically (recv outboxI)
    outputs <- readTVarIO outputV
    atomically (send (mconcat outputs) commands))


establishConnection :: TVar (Set HostName) -> TVar [Output (Set Command)] -> Output (Set Command) -> (Socket,SockAddr) -> IO ()
establishConnection peerV outputsV inboxO (socket,sockAddr) = do
    (connectionO,connectionI) <- spawn Single
    forkIO (runEffect (
        fromInput connectionI >-> Pipes.map (toStrict . encode) >-> toSocket socket))
    forkIO (runEffect (
        fromSocket socket 4096 >-> Pipes.map (fromJust . decode . fromStrict) >-> toOutput inboxO))
    atomically (modifyTVar peerV (insert (show sockAddr)))
    atomically (modifyTVar outputsV (connectionO:))


getPeers :: HostName -> IO (Set host)
getPeers = undefined