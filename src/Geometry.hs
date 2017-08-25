
module Geometry where

import Linear
import Linear.Affine
import Data.Map
import Control.Monad (when)
import Control.Monad.State

import Math

triangulate :: Polygon a -> [Triangle2 a]
triangulate (Polygon verts) =
    expressionTriangulateXYSorted (Polygon (sortByXy verts))
