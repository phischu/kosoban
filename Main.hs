module Main where

import Graphics.Gloss.Interface.IO.Game

import Network.Simple.TCP (
    withSocketsDo)

import Pipes.Concurrent

import Control.Concurrent.STM (
    newTVarIO,TVar,readTVarIO,readTVar,writeTVar)

import Data.Set (Set,empty,singleton,union)

import Control.Monad (forever,unless)

data Command = Command Time PlayerName Action
    deriving (Show,Read,Eq,Ord)

type Time = Integer

type PlayerName = String

data Action = East | North | West | South | Connect
    deriving (Show,Read,Eq,Ord)

main :: IO ()
main = withSocketsDo (do

    (inboxO,inboxI) <- spawn Single
    (outboxO,outboxI) <- spawn Single
    commandsVar <- newTVarIO empty
    connectionVar <- newTVarIO empty

    forkIO (forever (atomically (do
            Just incomingCommands <- recv inboxI
            currentCommands <- readTVar commandsVar
            let newCommands = union incomingCommands currentCommands
            unless (newCommands == currentCommands) (do
                writeTVar commandsVar newCommands
                send outboxO newCommands
                return ()))))

    playIO
        (InWindow "Kosoban" (600,600) (100,200))
        white
        20
        ()
        (render commandsVar)
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

