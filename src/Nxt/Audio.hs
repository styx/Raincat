module Nxt.Audio
    (Music,
     initAudio,
     loadMusic,
     playMusic) where

import qualified SDL.Mixer

type Music = SDL.Mixer.Music

-- initAudio
initAudio :: IO ()
initAudio = 
    let audio = SDL.Mixer.Audio
          { SDL.Mixer.audioFrequency = 44100
          , SDL.Mixer.audioFormat = SDL.Mixer.FormatS16_Sys
          , SDL.Mixer.audioOutput = SDL.Mixer.Stereo }
    in SDL.Mixer.openAudio audio 4096

-- loadMusic
loadMusic :: String -> IO Music
loadMusic = SDL.Mixer.load

-- playMusic
playMusic :: Music -> IO ()
playMusic m = do
    SDL.Mixer.setMusicVolume 50
    SDL.Mixer.playMusic SDL.Mixer.Forever m

