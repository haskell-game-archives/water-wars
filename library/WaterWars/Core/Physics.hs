module WaterWars.Core.Physics where

import           ClassyPrelude                     hiding ( Reader
                                                          , asks
                                                          )
import           WaterWars.Core.Game
import           WaterWars.Core.Physics.Constants
import           WaterWars.Core.Physics.Collision
import           Control.Eff
import           Control.Eff.Reader.Strict


jumpVector :: VelocityVector -> VelocityVector
jumpVector (VelocityVector x _) = VelocityVector x jumpForce

-- TODO: onGround states
runVector :: Bool -> RunDirection -> VelocityVector
runVector True  RunLeft  = VelocityVector (-runAccelerationGround) 0
runVector True  RunRight = VelocityVector runAccelerationGround 0
runVector False RunLeft  = VelocityVector (-runAccelerationAir) 0
runVector False RunRight = VelocityVector runAccelerationAir 0

gravityVector :: VelocityVector
gravityVector = VelocityVector 0 (-gravityForce)

blockLocationBelowFeet :: InGamePlayer -> BlockLocation
blockLocationBelowFeet InGamePlayer { playerLocation } =
    let Location (x, y) = playerLocation
    in  BlockLocation (round x, round $ y - 0.001 {- TODO: this should be a constant-})

gravityPlayer :: Bool -> InGamePlayer -> InGamePlayer
gravityPlayer True = id
gravityPlayer False = acceleratePlayer gravityVector

gravityProjectile :: Projectile -> Projectile
gravityProjectile = accelerateProjectile gravityVector

verticalDragPlayer :: Bool -> InGamePlayer -> InGamePlayer
verticalDragPlayer onGround player@InGamePlayer {..} =
    let VelocityVector vx vy = playerVelocity
        dragFactor = if onGround then verticalDragGround else verticalDragAir
    in  setPlayerVelocity (VelocityVector (vx * dragFactor) vy) player


isPlayerOnGround :: Member (Reader GameMap) e => InGamePlayer -> Eff e Bool
isPlayerOnGround InGamePlayer {..} = do
    blocks <- asks gameTerrain
    let Location (x, y) = playerLocation
    let blockBelowFeet  = BlockLocation (round x, round $ y - 0.001 {- TODO: this should be a constant-})
    return $ isSolidAt blocks blockBelowFeet

movePlayer :: Member (Reader GameMap) e => InGamePlayer -> Eff e InGamePlayer
movePlayer player@InGamePlayer {..} = do
    blocks <- asks gameTerrain
    let (targetLocation, newVelocity) =
            moveWithCollision blocks playerLocation playerVelocity
    return player { playerLocation = targetLocation
                  , playerVelocity = newVelocity
                  }
