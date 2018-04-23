module WaterWars.Server.GameLoop where

import ClassyPrelude

import System.Log.Logger

import Control.Concurrent

import WaterWars.Core.GameState
import WaterWars.Core.GameAction

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

import WaterWars.Network.Protocol


-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> TChan PlayerAction -> m ()
runGameLoop serverStateStm broadcastReadSide = forever $ do
    liftIO $ debugM "Server.Connection" "Start the game loop"
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        let newState = runGameTick gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    broadcastGameState connections gameState
    liftIO $ threadDelay 10000 -- TODO: this sleep is necessary


allGameTicks :: [Map Player Action] -> GameState -> [GameState]
allGameTicks [] s = [s]
allGameTicks (actions:rest) initialState =
    initialState : allGameTicks rest (runGameTick initialState actions)
