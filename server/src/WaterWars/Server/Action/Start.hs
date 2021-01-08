{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Server.Action.Start where

import ClassyPrelude hiding
  ( Reader,
    ask,
  )
import Control.Eff
import Control.Eff.Log
import qualified Control.Eff.Log as EffLog
import Control.Eff.Reader.Strict
import WaterWars.Core.Game
import WaterWars.Network.Protocol
import WaterWars.Server.Action.Util
import WaterWars.Server.Env

startGame ::
  (Member (Log Text) r, Member (Reader Env) r, MonadIO m, Lifted m r) =>
  Eff r ()
startGame = do
  ServerEnv {..} <- reader serverEnv
  let gameTick = gameTicks . gameState $ gameLoop
  EffLog.logE $ "Send the Game start message: " ++ tshow gameTick

  broadcastMessage GameStartMessage
