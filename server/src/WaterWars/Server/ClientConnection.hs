{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Server.ClientConnection where

import ClassyPrelude
import Control.Eff
import Control.Eff.Log
import qualified Control.Eff.Log as EffLog
import WaterWars.Network.Protocol
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.Events

clientGameThread ::
  -- | Logger implemetation
  Logger IO Text ->
  -- | Connection of the client
  Connection ->
  -- | Send Message to Eventloop
  (ClientMessage -> Eff '[Log Text, Lift IO] ()) ->
  -- | Reads action to send from a monadic function
  Eff '[Log Text, Lift IO] ServerMessage ->
  -- | Should never return
  IO ()
clientGameThread logger conn sendAction receiveAction =
  race_
    -- If any of these threads die, kill both threads and return, be careful for this swallows exceptions
    (clientReceive logger conn sendAction)
    (clientSend logger conn receiveAction)

clientReceive ::
  -- | Logger implemetation
  Logger IO Text ->
  -- | Connection of the client
  Connection ->
  -- | Send Message to Eventloop
  (ClientMessage -> Eff '[Log Text, Lift IO] ()) ->
  -- | Void or absurd, should never return
  IO ()
clientReceive logger conn sendAction =
  runLift
    . runLog logger
    . forever
    $ do
      -- Eff '[Log Text, Lift IO] ()
      -- EffLog.logE ("Wait for data message" :: Text)
      msg <- receive conn
      case msg of
        Left msg_ -> do
          EffLog.logE ("Could not read message" :: Text)
          EffLog.logE ("Could not read message: " ++ tshow msg_)
        Right playerAction -> do
          --EffLog.logE ("Read a message: " ++ tshow playerAction)
          sendAction playerAction
          return ()

-- TODO: should i sleep here for some time to avoid DOS-attack? yes

clientSend ::
  -- | Logger implemetation
  Logger IO Text ->
  -- | Connection of the client
  Connection ->
  -- | Reads action to send from a monadic function
  Eff '[Log Text, Lift IO] ServerMessage ->
  -- | Void or absurd, should never return
  IO ()
clientSend logger conn receiveAction =
  runLift
    . runLog logger
    . forever
    $ do
      -- Eff '[Log Text, Lift IO] ()
      -- EffLog.logE ("Wait for message" :: Text)
      cmd <- receiveAction
      send conn cmd
      return ()
