module Main
    () where

import Graphics.UI.GLUT
import Data.Maybe
import Control.OldException
import System.Exit
import Game.GameInput
import Game.GameMain
import Game.GameInit
import World.World
import Settings.DisplaySettings as DisplaySettings
import qualified Nxt.Graphics
import Game.GameGraphics
import Nxt.Audio
import Data.IORef
import Program.Program

main :: IO ()
main = do
    Nxt.Graphics.initWindow screenRes "Raincat"
    Nxt.Graphics.initGraphics screenResWidth screenResHeight

    worldState <- gameInit
    worldStateRef <- newIORef worldState

    displayCallback $= programDraw worldStateRef

    keyboardMouseCallback $= Just (gameInput (keysStateRef worldState))
    motionCallback $= Just (gameMotion (mousePosRef worldState))
    passiveMotionCallback $= Just (gameMotion (mousePosRef worldState))

    addTimerCallback 1 (programMain worldStateRef)

    mainLoop

exitMain :: IO ()
exitMain =
    throwIO $ ExitException ExitSuccess

