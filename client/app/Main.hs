{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Control.Monad.Except
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Options.Applicative
import Sound.ProteaAudio
import System.Remote.Monitoring
import WaterWars.Client.Network.Connection (NetworkConfig (..), connectionThread)
import WaterWars.Client.OptParse
import WaterWars.Client.Render.Display (renderIO)
import WaterWars.Client.Render.State
import WaterWars.Client.Render.Update (handleKeysIO, updateIO)

window :: Bool -> Display
window False = InWindow "Water Wars" (800, 600) (10, 10)
window True = FullScreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

opts :: ParserInfo Arguments
opts =
  info
    (argumentsParser <**> helper)
    ( fullDesc
        <> progDesc "Start an instance of the water-wars client."
        <> header "Fail Whale Brigade presents Water Wars."
    )

main :: IO ()
main = do
  Arguments {..} <- execParser opts

  case performanceMonitoring of
    Nothing -> return ()
    Just monitorPort -> void $ forkServer "localhost" monitorPort

  unless quiet $ do
    success <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless success $ fail "failed to initialize the audio system"

  resourcesEither <- runExceptT setup
  case resourcesEither of
    Left err -> say $ "Could not load texture. Cause: " ++ err
    Right resources -> do
      worldStm <- initializeState resources
      _ <-
        async {- Should never terminate -}
          ( connectionThread
              Nothing
              (NetworkConfig port (unpack hostname))
              worldStm
          )
      sample <- sampleFromFile "resources/sounds/Bubble_Game.ogg" 1.0
      _ <- soundLoop sample 1 1 0 1
      playIO
        (window fullScreen)
        backgroundColor
        fps
        worldStm
        renderIO
        handleKeysIO
        updateIO
      -- Will never be reached
      say "Goodbye, shutting down the Client!"
