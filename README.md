# Raincat
Game Creation Society - Fall 08

https://www.gamecreation.org/

### Screenshot
![Raincat screenshot](screenshot.png?raw=true "Raincat screenshot")

### Installation (Snap Store)
Raincat is available in the Snap Store at: https://snapcraft.io/raincat-game

It can be installed with:
```
sudo snap install raincat
```

After installation, Raincat can be launched from the Raincat desktop launcher, or with:
```
snap run raincat
```

### Compilation
You can compile locally with `runhaskell`:
```
runhaskell Setup.lhs --user configure
runhaskell Setup.lhs build
runhaskell Setup.lhs install
```

Compiling from Hackage with `cabal`:
```
cabal install Raincat
```

### Starting the game (compiled)
```
./raincat
```

### Changelog
```
Version 1.2:
    - Ported to SDL2
Version 1.1:
    - Changed initial item placement from click to select, click to place
      to drag n' drop.
    - Replaced eraser tool with right click to erase an item
    - Now properly detecting mouse clicks after window resize
Version 1.0:
    - Initial version
```

### Notes
The Raincat executable was compiled with GHC 6.12.1 on Arch Linux i686
and on Ubuntu 9.10. If you are having problems running the game, the
win32 build runs under wine.

Runtimes for the following libraries are assumed to be installed:
* GLUT
* OpenGL
* SDL2
* SDL2_image
* SDL2_mixer

### Troubleshooting
If you receive the error:
```
user error (Mix_LoadMUS SDL message: Module format not recognized)
```
when trying to run Raincat, this is because your SDL-mixer library
was not compiled with mp3 support. You will need to recompile that
with mp3 support or otherwise obtain a copy with mp3 support in order
to run the game.

### Project Team
```
Garrick Chin        - Project Leader/Programmer/Level Designer
Susan Lin           - Artist
SooHyun Jang        - Artist
Anthony Maurice     - Programmer
William Wang        - Programmer
Andrew Zheng        - Programmer
Rachel Berkowitz    - Music Composer
Spencer Ying        - Artist/Level Designer
Tal Stramer         - Level Editor Programmer
```

### Other Contributors
```
Mikhail Pobolovets  - Programmer
Sergei Trofimovich  - Programmer
Raahul Kumar        - Programmer
Alvaro F. García    - Programmer
```
