{-# LANGUAGE CPP #-}
module Nxt.Graphics
    (begin,
     end,
     initWindow,
     initGraphics,
     toGLdouble,
     fromGLdouble,
     loadTexture,
     freeTexture,
     drawTexture,
     drawTextureFlip,
     drawString,
     drawRect,
     drawPoly,
     worldTransform,
     cycleTextures,
     cycleTextures2,
     repeatTexturesN) where

import Control.Monad
import Graphics.UI.GLUT as GLUT hiding (windowSize, windowTitle)
import Graphics.Rendering.OpenGL as GL
import qualified SDL.Image as SDLImage hiding (loadTexture)
import qualified SDL.Vect
import qualified SDL.Video
import Nxt.Types hiding (rectX, rectY, rectWidth, rectHeight)
import Unsafe.Coerce

-- initWindow
initWindow :: Size -> String -> IO ()
initWindow windowSize windowTitle = do
    _ <- getArgsAndInitialize

    initialWindowSize $= windowSize
    initialDisplayMode $= [DoubleBuffered]

    _ <- createWindow windowTitle

    return ()

-- initGraphics
initGraphics :: GLdouble -> GLdouble -> IO ()
initGraphics screenResWidth screenResHeight = do
    blend $= Enabled
    blendFunc $= (GL.SrcAlpha, OneMinusSrcAlpha)
    shadeModel $= Flat

    matrixMode $= Projection
    loadIdentity
    ortho 0.0 screenResWidth 0.0 screenResHeight (-1.0) 0.0
    matrixMode $= Modelview 0

    return ()

-- begin
begin :: IO ()
begin =
    clear [ColorBuffer, DepthBuffer]

-- end
end :: IO ()
end = do
    swapBuffers
    flush

-- toGLdouble
toGLdouble :: a -> GLdouble
toGLdouble = unsafeCoerce

-- fromGLdouble
fromGLdouble :: a -> Double
fromGLdouble = unsafeCoerce

-- loadTexture (only specified to load PNGs)
loadTexture :: String -> IO Nxt.Types.Texture
loadTexture textureFilePath = do
    surface <- SDLImage.load textureFilePath

    SDL.Vect.V2 rawWidth rawHeight <- SDL.Video.surfaceDimensions surface
    let width = fromIntegral rawWidth
    let height = fromIntegral rawHeight
    let surfaceSize = TextureSize2D width height

    textureObj <- liftM head (genObjectNames 1)
    textureBinding Texture2D $= Just textureObj
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    surfacePixels <- SDL.Video.surfacePixels surface

    let pixelData = PixelData RGBA UnsignedByte surfacePixels
    texImage2D
#if MIN_VERSION_OpenGL(2,9,0)
          Texture2D
#else
          Nothing
#endif
          NoProxy 0 RGBA' surfaceSize 0 pixelData

    SDL.Video.freeSurface surface

    return (Nxt.Types.Texture width height textureObj)

-- freeTexture
freeTexture :: Nxt.Types.Texture -> IO ()
freeTexture tex =
    deleteObjectNames [textureObject tex]

-- drawTexture
drawTexture :: Double -> Double -> Nxt.Types.Texture -> GLdouble -> IO ()
drawTexture x y tex alpha =
    drawTextureFlip x y tex alpha False

-- drawTextureFlip
drawTextureFlip :: Double -> Double -> Nxt.Types.Texture -> GLdouble -> Bool -> IO ()
drawTextureFlip x y tex alpha fliped = do
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just (textureObject tex)

    let texWidth = fromIntegral $ textureWidth tex
        texHeight = fromIntegral $ textureHeight tex

    let texCoord2f = texCoord :: TexCoord2 GLdouble -> IO ()
        vertex3f = vertex :: Vertex3 GLdouble -> IO ()
        color4f = color :: Color4 GLdouble -> IO ()
        col = color4f (Color4 (1.0::GLdouble) (1.0::GLdouble) (1.0::GLdouble) alpha)

    let texCoordX = if fliped then (-1) else 1
        x' = toGLdouble x
        y' = toGLdouble y

    renderPrimitive Quads $ do
        texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 x' y' 0.0); col
        texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 x' (y' + texHeight) 0.0); col
        texCoord2f (TexCoord2 texCoordX 0); vertex3f (Vertex3 (x' + texWidth) (y' + texHeight) 0.0); col
        texCoord2f (TexCoord2 texCoordX 1); vertex3f (Vertex3 (x' + texWidth) y' 0.0); col

    texture Texture2D $= Disabled

-- drawString (using Helvetica 12pt font)
drawString :: GLfloat -> GLfloat -> String -> Color4 GLfloat -> IO ()
drawString x y string col = do
    color col
    currentRasterPosition $= Vertex4 x y (0.0::GLfloat) (1.0::GLfloat)
    renderString Helvetica12 string

-- drawRect
drawRect :: Nxt.Types.Rect -> Color4 GLdouble -> IO ()
drawRect (Rect rectX rectY rectWidth rectHeight) rectColor = do
    let rX = toGLdouble rectX
        rY = toGLdouble rectY
        rW = toGLdouble rectWidth
        rH = toGLdouble rectHeight
        rectVertices = [Vertex3 rX rY 0.0,
                        Vertex3 (rX + rW) rY 0.0,
                        Vertex3 (rX + rW) (rY + rH) 0.0,
                        Vertex3 rX (rY + rH) 0.0]

    renderPrimitive Quads $ do
        mapM_ color  [rectColor]
        mapM_ vertex rectVertices

-- drawPoly
drawPoly :: Nxt.Types.Poly -> Color4 GLdouble -> IO ()
drawPoly (Poly _ points) polyColor = do
    let polyVerts = map (\(x,y) -> Vertex3 (toGLdouble x) (toGLdouble y) (0.0::GLdouble)) points

    renderPrimitive Polygon $ do
        mapM_ color [polyColor]
        mapM_ vertex polyVerts

-- worldTransform
worldTransform :: Double -> Double -> IO ()
worldTransform worldX worldY = do
    loadIdentity
    translate (Vector3 (toGLdouble worldX) (toGLdouble worldY) 0.0)

-- cycleTextures
cycleTextures :: String -> Int -> Int -> IO [Nxt.Types.Texture]
cycleTextures filePath frames frameTime = do
    texLists <- mapM (\n -> Nxt.Graphics.loadTexture (filePath ++ show n ++ ".png")) [1..frames]
    let textures = cycle $ foldr ((++) . replicate frameTime) [] texLists

    return textures

-- cycleTextures2
cycleTextures2 :: String -> Int -> Int -> Int -> IO [Nxt.Types.Texture]
cycleTextures2 filePath frames lastFrame frameTime = do
    texLists <- mapM (\n -> Nxt.Graphics.loadTexture (filePath ++ show n ++ ".png")) [1..frames]
    texLists2 <- Nxt.Graphics.loadTexture (filePath ++ show lastFrame ++ ".png");
    let textures = foldr ((++) . replicate frameTime) (repeat texLists2) texLists

    return textures

-- repeatTexturesN
repeatTexturesN :: String -> Int -> Int -> Int -> Int -> Int -> Int -> IO [Nxt.Types.Texture]
repeatTexturesN filePath frames startRepeat endRepeat nRepeats lastFrame frameTime = do
    texLists <- mapM (\n -> Nxt.Graphics.loadTexture (filePath ++ show n ++ ".png")) [1..frames]
    repeatTexLists <- mapM (\n -> Nxt.Graphics.loadTexture (filePath ++ show n ++ ".png")) [startRepeat..endRepeat]
    endTexLists <- mapM (\n -> Nxt.Graphics.loadTexture (filePath ++ show n ++ ".png")) [(endRepeat + 1)..lastFrame]
    let textures = replicate 60 (last endTexLists)
                   ++
                   foldr ((++) . replicate frameTime) [] texLists
                   ++
                   take (nRepeats * frameTime * (endRepeat - startRepeat)) (cycle $ foldr ((++) . replicate frameTime) [] repeatTexLists)
                   ++
                   foldr ((++) . replicate frameTime) (repeat $ last endTexLists) endTexLists

    return textures

