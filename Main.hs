module Main where

import Graphics.Gloss.Interface.IO.Game

import Network.Simple.TCP

import Control.Concurrent.STM

import Data.Set (Set,empty)

data Command = Command Time PlayerName Action
    deriving (Show,Read,Eq,Ord)

type Time = Integer

type PlayerName = String

data Action = East | North | West | South | Connect
    deriving (Show,Read,Eq,Ord)

main :: IO ()
main = withSocketsDo (do
    commandsVar <- atomically (newTVar empty)
    commandVar <- atomically newEmptyTMVar
    playIO
        (InWindow "Kosoban" (600,600) (100,200))
        white
        20
        ()
        (const (fmap render (atomically (readTVar commandVar))))
        (handle commandVar
        step)

render :: Set Command -> Picture
render = undefined

handle :: Event -> () -> IO ()
handle = undefined

step :: Float -> () -> IO ()
step = undefined
