module Rope where

import Mesh
import Linear

strandSection :: Face Float
strandSection = regularNgon 6 0.01

strand :: Int -> Int -> Float -> Mesh Float
strand _ 0 r = extrudeSegmented 100 strandSection (V3 0 0 10)
strand s n r = twist bundle (V3 0 0 1) (V3 0 0 0) (if even n then (2.0 * pi / fromIntegral n) else -(2.0 * pi/ fromIntegral n))
             where bundle = mergeAll (translate subStrand <$> strandOrigins)
                   strandOrigins = regularNgonVertices s (fromIntegral n * r / 3.5)
                   subStrand = strand (2*s) (n-1) (r)

rope :: Mesh Float
rope = (strand 3 3 0.2)

twist :: Mesh Float -> V3 Float -> V3 Float -> Float -> Mesh Float
twist mesh axis origin angle = vertexMap trans mesh
    where trans = \vert -> let t = dot (vert - origin) axis / (norm axis)
                               rot = axisAngle axis (t * angle)
                               in rotate rot (vert - origin) + origin
