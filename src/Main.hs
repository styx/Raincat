module Main
    () where

import Graphics.UI.GLUT
import System.Exit
import Game.GameInput
import Game.GameInit
import World.World
import Settings.DisplaySettings as DisplaySettings
import qualified Nxt.Graphics as NG
import Data.IORef
import Program.Program

main :: IO ()
main = do
    NG.initWindow screenRes "Raincat"
    NG.initGraphics screenResWidth screenResHeight

    worldState <- gameInit
    worldStateRef <- newIORef worldState

    displayCallback $= programDraw worldStateRef

    keyboardMouseCallback $= Just (gameInput (keysStateRef worldState))
    motionCallback $= Just (gameMotion (mousePosRef worldState))
    passiveMotionCallback $= Just (gameMotion (mousePosRef worldState))

    addTimerCallback 1 (programMain worldStateRef)

    mainLoop

    exitWith ExitSuccess
