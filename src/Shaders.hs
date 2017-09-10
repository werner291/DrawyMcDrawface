module Shaders where

import Graphics.GPipe
import Control.Lens
import Math (lookAt')
import Data.Word (Word32)
import Data.Int

windowWidth = 800
windowHeight = 600

--------------------------- time --- window size - modelTranslation
type GlobalUniformFormat = (B Float, B2 Int32, V4 (B4 Float))

simpleNormalShader :: (DepthRenderable ds) =>
  (Window os RGBFloat ds) ->
  Buffer os (Uniform GlobalUniformFormat) ->
  Shader os (PrimitiveArray Triangles (B4 Float, B3 Float)) ()

lightDir = V3 (fromIntegral 0) (fromIntegral 0) (fromIntegral (1))

simpleNormalShader win globalUniformBuffer = do
  primitives <- toPrimitiveStream id
  (time, V2 sizeX sizeY, modelMat) <- getUniform (const (globalUniformBuffer,0))
  let viewMat = lookAt' (V3 (8 * cos (time)) (8 * sin (time)) 5) (V3 0 0 0) (V3 0 0 1)
      aspect = (toFloat sizeX) / (toFloat sizeY)
      projection = perspective (pi * 0.50) aspect 0.1 100
      transformedPrimitives = fmap (\(pos,clr) -> let diffuseLight = (0.5 + abs (0.5 * dot lightDir clr)) in (projection !*! viewMat !*! modelMat !* pos , V3 diffuseLight diffuseLight diffuseLight)) primitives
  fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 windowWidth windowHeight), DepthRange 0 1)) transformedPrimitives
  let fS2 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream
  drawWindowColorDepth (const (win, ContextColorOption NoBlending (V3 True True True), DepthOption Lequal True)) fS2