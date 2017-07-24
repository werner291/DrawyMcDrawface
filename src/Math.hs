module Math where

import Linear
import Control.Lens

-- | Simplified version of the lookAt function from
-- | linear that does not handle near-zero vectors.
-- | It replaces the call to "normalize" with "signorm"
lookAt' eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

transMat (V3 x y z) =
  V4 (V4 1 0 0 x)
     (V4 0 1 0 y)
     (V4 0 0 1 z)
     (V4 0 0 0 1)

sliding :: Int -> Int -> [a]-> [[a]]
sliding _ _ [] = []
sliding size step xs = take size xs : (sliding size step (drop step xs))
