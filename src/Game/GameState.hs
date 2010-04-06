module Game.GameState
    (GameState(GameRunningState,MainMenuState,HowtoMenuState,PostVictoryState)) where

data GameState = GameRunningState | MainMenuState | HowtoMenuState | PostVictoryState

