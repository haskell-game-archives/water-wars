{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Core.TerrainsUtils where

import ClassyPrelude
import Data.Array.IArray
import WaterWars.Core.Game.Map

smallBounds :: (BlockLocation, BlockLocation)
smallBounds = (BlockLocation (-2, -2), BlockLocation (2, 2))

terrainEmpty :: Terrain
terrainEmpty = Terrain $ listArray smallBounds $ replicate 25 NoBlock

terrainWithBlockAt :: (Int, Int) -> Terrain
terrainWithBlockAt location =
  Terrain $
    accumArray
      (\_ x -> x)
      NoBlock
      smallBounds
      [(BlockLocation location, SolidBlock Middle)]

terrainWithBlocksAt :: [(Int, Int)] -> Terrain
terrainWithBlocksAt locations =
  Terrain $
    accumArray (\_ x -> x) NoBlock smallBounds $
      map
        (\l -> (BlockLocation l, SolidBlock Middle))
        locations
