module Input.InputState
    (MousePos(MousePos),
     mouseX,
     mouseY,
     KeysState(KeysState),
     leftKeyDown,
     rightKeyDown,
     downKeyDown,
     upKeyDown,
     lMousePrevDown,
     lMouseDown,
     rMouseDown,
     spaceKeyDown,
     escKeyDown,
     translateMousePos) where

import Unsafe.Coerce
import Graphics.Rendering.OpenGL
import Nxt.Graphics
import Settings.DisplaySettings

data KeysState = KeysState
    {
        leftKeyDown         :: Bool,
        rightKeyDown        :: Bool,
        downKeyDown         :: Bool,
        upKeyDown           :: Bool,
        lMousePrevDown      :: Bool,
        lMouseDown          :: Bool,
        rMouseDown          :: Bool,
        spaceKeyDown        :: Bool,
        escKeyDown          :: Bool
    }

data MousePos = MousePos
    {
        mouseX  :: Int,
        mouseY  :: Int
    }

-- fromGLsizei
fromGLsizei :: a -> Int
fromGLsizei = unsafeCoerce

-- translateMousePos
translateMousePos :: MousePos -> GLsizei -> GLsizei -> (Double, Double)
translateMousePos (MousePos x y) winW winH =
    let x' = fromIntegral x
        sW' = (fromGLdouble screenResWidth)::Double
        wW' = (fromIntegral (fromGLsizei winW))
        y' = fromIntegral y
        sH' = (fromGLdouble screenResHeight)::Double
        wH' = (fromIntegral (fromGLsizei winH))
        in (x' * (sW' / wW'),
            sH' - ((sH' - y') * (sH' / wH')))
