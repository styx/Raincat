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
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL.Image as SDLImage
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video
import Nxt.Types
import Unsafe.Coerce

-- initWindow
initWindow :: Size -> [Char] -> IO ()
initWindow windowSize windowTitle = do
    getArgsAndInitialize

    initialWindowSize $= windowSize
    initialDisplayMode $= [DoubleBuffered]
    
    createWindow windowTitle

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
begin = do
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
loadTexture :: [Char] -> IO (Nxt.Types.Texture)
loadTexture textureFilePath = do
    surface <- SDLImage.loadTyped textureFilePath SDLImage.PNG

    let width = fromIntegral (surfaceGetWidth surface)
    let height = fromIntegral (surfaceGetHeight surface)
    let surfaceSize = TextureSize2D width height

    textureObj <- liftM head (genObjectNames 1)
    textureBinding Texture2D $= Just textureObj
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    surfacePixels <- surfaceGetPixels surface

    let pixelData = PixelData RGBA UnsignedByte surfacePixels
    texImage2D Nothing NoProxy 0 RGBA' surfaceSize 0 pixelData

    freeSurface surface

    return (Nxt.Types.Texture width height textureObj)

-- freeTexture
freeTexture :: Nxt.Types.Texture -> IO ()
freeTexture tex = do
    deleteObjectNames ([textureObject tex])

-- drawTexture
drawTexture :: Double -> Double -> Nxt.Types.Texture -> GLdouble -> IO ()
drawTexture x y tex alpha = do
    drawTextureFlip x y tex alpha False

-- drawTextureFlip
drawTextureFlip :: Double -> Double -> Nxt.Types.Texture -> GLdouble -> Bool -> IO ()
drawTextureFlip x y tex alpha flip = do
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just (textureObject tex)

    let texWidth = fromIntegral $ textureWidth tex
        texHeight = fromIntegral $ textureHeight tex

    let texCoord2f = texCoord :: TexCoord2 GLdouble -> IO ()
        vertex3f = vertex :: Vertex3 GLdouble -> IO ()
        color4f = color :: Color4 GLdouble -> IO ()
        col = color4f (Color4 (1.0::GLdouble) (1.0::GLdouble) (1.0::GLdouble) alpha)

    let texCoordX = if flip then (-1) else 1
        x' = toGLdouble x
        y' = toGLdouble y

    renderPrimitive Quads $ do
        texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 x' y' 0.0); col
        texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 x' (y' + texHeight) 0.0); col
        texCoord2f (TexCoord2 texCoordX 0); vertex3f (Vertex3 (x' + texWidth) (y' + texHeight) 0.0); col
        texCoord2f (TexCoord2 texCoordX 1); vertex3f (Vertex3 (x' + texWidth) y' 0.0); col

    texture Texture2D $= Disabled

-- drawString (using Helvetica 12pt font)
drawString :: GLfloat -> GLfloat -> [Char] -> Color4 GLfloat -> IO ()
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
        mapM_ (\x -> color x) [rectColor]
        mapM_ (\x -> vertex x) rectVertices

-- drawPoly
drawPoly :: Nxt.Types.Poly -> Color4 GLdouble -> IO ()
drawPoly (Poly sides points) polyColor = do
    let polyVerts = map (\(x,y) -> Vertex3 (toGLdouble x) (toGLdouble y) (0.0::GLdouble)) points

    renderPrimitive Polygon $ do
        mapM_ (\x -> color x) [polyColor]
        mapM_ (\x -> vertex x) polyVerts

-- worldTransform
worldTransform :: Double -> Double -> IO ()
worldTransform worldX worldY = do
    loadIdentity
    translate (Vector3 (toGLdouble worldX) (toGLdouble worldY) 0.0)

-- cycleTextures
cycleTextures :: [Char] -> Int -> Int -> IO ([Nxt.Types.Texture])
cycleTextures filePath frames frameTime = do
    texLists <- sequence $ map (\n -> Nxt.Graphics.loadTexture (filePath ++ (show n) ++ ".png")) [1..frames]
    let textures = cycle $ foldr (\tex texList -> (replicate frameTime tex) ++ (texList)) [] texLists

    return (textures)

-- cycleTextures2
cycleTextures2 :: [Char] -> Int -> Int -> Int -> IO ([Nxt.Types.Texture])
cycleTextures2 filePath frames lastFrame frameTime = do
	texLists <- sequence $ map (\n -> Nxt.Graphics.loadTexture (filePath ++ (show n) ++ ".png")) [1..frames]
	texLists2 <- Nxt.Graphics.loadTexture (filePath ++ (show lastFrame) ++ ".png");
	let textures = foldr (\tex texList -> (replicate frameTime tex) ++ (texList)) (repeat texLists2) texLists

	return (textures)

-- repeatTexturesN
repeatTexturesN :: [Char] -> Int -> Int -> Int -> Int -> Int -> Int -> IO ([Nxt.Types.Texture])
repeatTexturesN filePath frames startRepeat endRepeat nRepeats lastFrame frameTime = do
	texLists <- sequence $ map (\n -> Nxt.Graphics.loadTexture (filePath ++ (show n) ++ ".png")) [1..frames]
	repeatTexLists <- sequence $ map (\n -> Nxt.Graphics.loadTexture (filePath ++ (show n) ++ ".png")) [startRepeat..endRepeat]
	endTexLists <- sequence $ map (\n -> Nxt.Graphics.loadTexture (filePath ++ (show n) ++ ".png")) [(endRepeat + 1)..lastFrame]
	let textures = (take 60 (repeat $ last endTexLists))
                   ++
                   (foldr (\tex texList -> (replicate frameTime tex) ++ (texList)) [] texLists)
                   ++
                   (take (nRepeats * frameTime * (endRepeat - startRepeat)) $ cycle $ foldr (\tex texList -> (replicate frameTime tex) ++ (texList)) [] repeatTexLists)
                   ++
                   (foldr (\tex texList -> (replicate frameTime tex) ++ (texList)) (repeat $ last endTexLists) endTexLists)

	return (textures)
