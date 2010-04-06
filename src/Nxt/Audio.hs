module Nxt.Audio
    (Music,
     initAudio,
     loadMusic,
     playMusic) where

import qualified Graphics.UI.SDL.Mixer.General as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.Music as SDL.Mixer.Music
import qualified Graphics.UI.SDL.Mixer.Types as SDL.Mixer.Types

type Music = SDL.Mixer.Types.Music

-- initAudio
initAudio :: IO ()
initAudio = SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16Sys 2 4096

-- loadMusic
loadMusic :: [Char] -> IO (Music)
loadMusic = SDL.Mixer.Music.loadMUS

-- playMusic
playMusic :: Music -> IO ()
playMusic m = do
    SDL.Mixer.Music.setMusicVolume 50
    SDL.Mixer.Music.playMusic m (-1)
