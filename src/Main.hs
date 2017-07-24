{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent (threadDelay)
import Control.Monad.Exception (MonadException)
--import Control.Lens

import Math (transMat)
import Shaders (simpleNormalShader,GlobalUniformFormat)
import Primitives (cubeVerts)
import Abstract

-- Application

targetFps :: Int = 60
targetInterval = div 1000000 targetFps

main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "Hello world!")

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (length cubeVerts)
    writeBuffer vertexBuffer 0 cubeVerts

    globalUniformBuffer :: Buffer os (Uniform GlobalUniformFormat) <- newBuffer 1

    shader <- compileShader $ simpleNormalShader win globalUniformBuffer

    let abstractScene = stackOfCubes 5

    let scene = flattenDrawable $ drawableFromConcept abstractScene

    loop (showFrame vertexBuffer shader win globalUniformBuffer scene) win 0.0

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
     [Drawable] ->
     Float ->
     ContextT GLFW.Handle os m ()

showFrame vertexBuffer shader win timeUniformBuffer scene time = do

  render $ do
    clearWindowColor win (V3 0 0 0)
    clearWindowDepth win 1 -- 1 is the far plane in NDC

  let pos = map (\a -> a - (V3 0.1 0.1 10.1)) $ map position scene

  mapM (drawAtPos vertexBuffer shader timeUniformBuffer time) pos

  swapWindowBuffers win

drawAtPos vertexBuffer shader timeUniformBuffer time pos = do
  writeBuffer timeUniformBuffer 0 [(time, V2 800 600, transMat pos)]

  render $ do
    vertexArray <- newVertexArray vertexBuffer
    shader $ toPrimitiveArray TriangleList vertexArray
