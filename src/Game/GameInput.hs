module Game.GameInput
    (gameInput,
     gameMotion) where

import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Input.InputState as InputState
import Settings.DisplaySettings as DisplaySettings

-- gameMotion
gameMotion :: IORef InputState.MousePos -> Position -> IO ()
gameMotion mousePosRef (Position posX posY) =
    writeIORef mousePosRef (InputState.MousePos (fromIntegral posX) (truncate DisplaySettings.screenResHeight - fromIntegral posY))

-- gameInput
gameInput :: IORef InputState.KeysState -> Key -> KeyState -> Modifiers -> Position -> IO ()
gameInput keysStateRef key state _ _ = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {lMousePrevDown = lMouseDown keysState})
    keyboardAct keysStateRef key state

-- keyboardAct
keyboardAct :: IORef InputState.KeysState -> Key -> KeyState -> IO ()

-- up arrow key
keyboardAct keysStateRef (SpecialKey KeyUp) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {upKeyDown = True})

keyboardAct keysStateRef (SpecialKey KeyUp) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {upKeyDown = False})

-- down arrow key
keyboardAct keysStateRef (SpecialKey KeyDown) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {downKeyDown = True})

keyboardAct keysStateRef (SpecialKey KeyDown) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {downKeyDown = False})

-- left arrow key
keyboardAct keysStateRef (SpecialKey KeyLeft) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {leftKeyDown = True})

keyboardAct keysStateRef (SpecialKey KeyLeft) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {leftKeyDown = False})

-- right arrow key
keyboardAct keysStateRef (SpecialKey KeyRight) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {rightKeyDown = True})

keyboardAct keysStateRef (SpecialKey KeyRight) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {rightKeyDown = False})

-- space key
keyboardAct keysStateRef (Char ' ') Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {spaceKeyDown = True})

keyboardAct keysStateRef (Char ' ') Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {spaceKeyDown = False})

-- esc key
keyboardAct keysStateRef (Char '\ESC') Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {escKeyDown = True})

keyboardAct keysStateRef (Char '\ESC') Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {escKeyDown = False})

-- quit key
keyboardAct _ (Char 'q') Down = do
    exitWith ExitSuccess

-- left mouse button
keyboardAct keysStateRef (MouseButton LeftButton) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {lMouseDown = True})

keyboardAct keysStateRef (MouseButton LeftButton) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {lMouseDown = False})

-- right mouse button
keyboardAct keysStateRef (MouseButton RightButton) Down = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {rMouseDown = True})

keyboardAct keysStateRef (MouseButton RightButton) Up = do
    keysState <- readIORef keysStateRef
    writeIORef keysStateRef (keysState {rMouseDown = False})

keyboardAct _ _ _ = return ()

