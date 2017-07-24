module Primitives where

import Linear

r = 0.5

cubeVerts :: [(V4 Float, V3 Float)]
cubeVerts = concat $ map (\quad -> (take 3 quad) ++ (take 3 $ reverse quad))
            [ [(V4 r x y 1,    V3   1.0   0.0    0.0 ) | x <- [-r,r], y <- [-r,r]]
            , [(V4 x r y 1,    V3   0.0   1.0    0.0 ) | x <- [-r,r], y <- [-r,r]]
            , [(V4 x y r 1,    V3   0.0   0.0    1.0 ) | x <- [-r,r], y <- [-r,r]]
            , [(V4 (-r) x y 1, V3  (1.0)  0.0    0.0 ) | x <- [-r,r], y <- [-r,r]]
            , [(V4 x (-r) y 1, V3   0.0  (1.0)   0.0 ) | x <- [-r,r], y <- [-r,r]]
            , [(V4 x y (-r) 1, V3   0.0   0.0   (1.0)) | x <- [-r,r], y <- [-r,r]] ]
