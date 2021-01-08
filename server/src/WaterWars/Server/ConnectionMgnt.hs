{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Server.ConnectionMgnt
  ( ClientConnection (..),
    newClientConnection,
    send,
    receive,
  )
where

import ClassyPrelude
import qualified Network.WebSockets as WS
import WaterWars.Network.Protocol

data ClientConnection a b = ClientConnection
  { -- | Session id, uniquely identifies players
    connectionId :: Text,
    -- | Abstraction over an connection handle
    connection :: WS.Connection,
    -- | Client threads read from this channel
    readChannel :: TQueue a,
    -- | Client threads write to this channel
    writeChannel :: TQueue b
  }

instance Eq (ClientConnection a b) where
  c1 == c2 = connectionId c1 == connectionId c2

instance Ord (ClientConnection a b) where
  c1 <= c2 = connectionId c1 <= connectionId c2

instance Show (ClientConnection a b) where
  show ClientConnection {..} = "ClientConnection { connectionId = " ++ show connectionId ++ "}"

send :: MonadIO m => ClientConnection a b -> ServerMessage -> m ()
send conn toSend = do
  let msg = serialize toSend
  liftIO $ WS.sendTextData (connection conn) msg

receive :: MonadIO m => ClientConnection a b -> m (Either String ClientMessage)
receive conn = do
  msg <- liftIO $ WS.receiveData (connection conn)
  return $ deserialize msg

newClientConnection ::
  Text -> WS.Connection -> TQueue a -> TQueue b -> ClientConnection a b
newClientConnection = ClientConnection
