{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Server.Action.Util where

import ClassyPrelude hiding
  ( Reader,
    ask,
  )
import Control.Eff
import Control.Eff.Reader.Strict
import WaterWars.Core.Game
import WaterWars.Network.Protocol
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.Env
import WaterWars.Server.Events

broadcastMessage ::
  (Member (Reader Env) r, MonadIO m, Lifted m r) =>
  ServerMessage ->
  Eff r ()
broadcastMessage serverMessage = do
  session <- reader (connectionMap . networkEnv)
  forM_ (session :: Map Player Connection) $
    \conn -> atomically $ writeTQueue (readChannel conn) serverMessage
