module Program.Program
    (programMain,
     programDraw) where

import Data.IORef
import World.World
import Game.GameGraphics
import Game.GameState
import Menu.Menu
import Game.GameMain
import Menu.PostVictory

mainCallback :: (IORef WorldState -> IO ())
mainCallback worldStateRef = do
    worldState <- readIORef worldStateRef

    case gameState worldState of
         GameRunningState   -> gameMain worldStateRef mainCallback
         MainMenuState      -> menuMain worldStateRef mainCallback
         HowtoMenuState     -> howtoMain worldStateRef mainCallback
         PostVictoryState   -> postVictoryMain worldStateRef mainCallback

programMain :: IORef WorldState -> IO ()
programMain = mainCallback

programDraw :: IORef WorldState -> IO ()
programDraw worldStateRef = do
    worldState <- readIORef worldStateRef

    case gameState worldState of
         GameRunningState   -> gameDraw worldStateRef
         MainMenuState      -> menuDraw worldStateRef
         HowtoMenuState     -> howtoDraw worldStateRef
         PostVictoryState   -> postVictoryDraw worldStateRef

