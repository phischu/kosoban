module Main where

import Graphics.Gloss.Interface.IO.Game

import Network.Simple.TCP (
    withSocketsDo)

import Pipes.Concurrent

import Data.Set (Set,empty,singleton)

data Command = Command Time PlayerName Action
    deriving (Show,Read,Eq,Ord)

type Time = Integer

type PlayerName = String

data Action = East | North | West | South | Connect
    deriving (Show,Read,Eq,Ord)

main :: IO ()
main = withSocketsDo (do
    (messagesO,messagesI) <- spawn Single
    (commandsO,commandsI) <- spawn (Latest empty)
    playIO
        (InWindow "Kosoban" (600,600) (100,200))
        white
        20
        ()
        (render commandsI)
        (handle messagesO)
        (const return))

render :: Input (Set Command) -> () -> IO Picture
render commandsI () = do
    commands <- atomically (recv commandsI)
    return (circle 20)

handle :: Output (Set Command) -> Event -> () -> IO ()
handle messagesO event () = do
    let command = undefined
    atomically (send messagesO (singleton command))
    return ()

