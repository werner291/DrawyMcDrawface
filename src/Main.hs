{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent (threadDelay)
import Control.Monad.Exception (MonadException)
import Data.Maybe (fromJust)
import Control.Monad.Random
import Control.Monad.Supply
--import Control.Lens

import Math (transMat)
import Shaders (simpleNormalShader,GlobalUniformFormat)

import Mesh
import Building
import Rope

-- Application

targetFps :: Int = 60
targetInterval = div 1000000 targetFps

extendV3Position (V3 x y z) = V4 x y z 1

normal (Triangle a b c) = signorm $ (a-b) `cross` (a-c)

vertices :: Mesh Float -> [(V4 Float, V3 Float)]
vertices (Mesh faces) = concat $ map (\(Triangle a b c) ->
  [(extendV3Position a, normal (Triangle a b c)),
   (extendV3Position b, normal (Triangle a b c)),
   (extendV3Position c, normal (Triangle a b c))]) (concat (map triangulateFanFromFirst faces))

main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Hello world!")

    buildingMesh <- liftIO (evalRandIO (evalSupplyT building [0..]))

    let cubeVerts = Main.vertices buildingMesh

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (length cubeVerts)
    writeBuffer vertexBuffer 0 cubeVerts

    guiVertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (length cubeVerts)
    writeBuffer guiVertexBuffer 0 cubeVerts

    globalUniformBuffer :: Buffer os (Uniform GlobalUniformFormat) <- newBuffer 1

    shader <- compileShader $ simpleNormalShader win globalUniformBuffer

    loop (showFrame vertexBuffer shader win globalUniformBuffer) win 0.0

loop :: (Color c Float ~ V3 a1,
         ContextColorFormat c,
         DepthRenderable ds,
         MonadException m,
         MonadIO m,
         Num a1) =>
     (Float -> ContextT GLFW.Handle os m ())-> Window os c ds -> Float
     -> ContextT GLFW.Handle os m ()

loop renderAction win time = do
  renderAction time
  liftIO $ threadDelay targetInterval
  shouldClose <- GLFW.windowShouldClose win
  unless (shouldClose == Just True) $
      loop renderAction win (time + 0.05)

showFrame :: (Color c Float ~ V3 a1,
         ContextColorFormat c,
         DepthRenderable ds,
         MonadException m,
         MonadIO m,
         Num a1) =>
     Buffer os a ->
     (PrimitiveArray Triangles a -> Render os ()) ->
     Window os c ds ->
     Buffer os (Uniform GlobalUniformFormat) ->
     Float ->
     ContextT GLFW.Handle os m ()

showFrame vertexBuffer shader win timeUniformBuffer time = do

  writeBuffer timeUniformBuffer 0 [(time, V2 800 600, transMat (V3 0 0 0))]

  render $ do
    clearWindowColor win (V3 0 0 0)
    clearWindowDepth win 1 -- 1 is the far plane in NDC
    vertexArray <- newVertexArray vertexBuffer
    shader $ toPrimitiveArray TriangleList vertexArray

  swapWindowBuffers win
