{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WaterWars.Core.Physics.Utils where

import ClassyPrelude
import WaterWars.Core.Game

velocityBoundX :: Float -> VelocityVector -> VelocityVector
velocityBoundX maxX v@(VelocityVector vx vy) =
  if abs vx <= maxX then v else VelocityVector (boundedBy (- maxX, maxX) vx) vy

distanceFromLine' :: Location -> VelocityVector -> BlockLocation -> Float
distanceFromLine' (Location (x, y)) (VelocityVector vx vy) (BlockLocation (bx, by)) =
  abs $ (x - fromIntegral bx) * nx + (y - fromIntegral by) * ny
  where
    nx = vy
    ny = - vx

-- TODO: test for that..
collisionPointsOfPlayer :: InGamePlayer -> [Location]
collisionPointsOfPlayer InGamePlayer {..} =
  [Location (x0, y0), Location (x1, y0), Location (x0, y1), Location (x1, y1)]
    ++ [Location (x0, y) | y <- ys] -- left border
    ++ [Location (x1, y) | y <- ys] -- right border
    ++ [Location (x, y0) | x <- xs] -- bottom border
    ++ [Location (x, y1) | x <- xs] -- top border
  where
    Location (x_, y_) = playerLocation
    numX = fromIntegral (1 + floor playerWidth :: Int)
    numY = fromIntegral (1 + floor playerHeight :: Int)
    dx = playerWidth / numX
    dy = playerHeight / numY
    x0 = x_ - playerWidth / 2
    x1 = x_ + playerWidth / 2
    y0 = y_
    y1 = y_ + playerHeight
    xs = [x0 + dx * k | k <- [1 .. numX - 1]]
    ys = [y0 + dy * k | k <- [1 .. numY - 1]]

-- TODO: better algorithm && test
bottomPointsOfPlayer :: InGamePlayer -> [Location]
bottomPointsOfPlayer p@InGamePlayer {playerLocation = Location (_, py)} =
  filter (\(Location (_, y)) -> y == py) . collisionPointsOfPlayer $ p

rightPointsOfPlayer :: InGamePlayer -> [Location]
rightPointsOfPlayer p@InGamePlayer {playerLocation = Location (px, _), playerWidth} =
  filter (\(Location (x, _)) -> x == px + playerWidth / 2) . collisionPointsOfPlayer $ p

leftPointsOfPlayer :: InGamePlayer -> [Location]
leftPointsOfPlayer p@InGamePlayer {playerLocation = Location (px, _), playerWidth} =
  filter (\(Location (x, _)) -> x == px - playerWidth / 2) . collisionPointsOfPlayer $ p

-- bound velocity vector to be max 0.5 in both directions
boundVelocityVector :: (Float, Float) -> VelocityVector -> VelocityVector
boundVelocityVector (maxX, maxY) v@(VelocityVector vx vy) =
  if abs vx <= maxX && abs vy <= maxY
    then v
    else
      VelocityVector
        (boundedBy (- maxX, maxX) vx)
        (boundedBy (- maxY, maxY) vy)
