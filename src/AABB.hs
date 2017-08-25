module AABB where

data AABB a = AABB { xMin :: a
                   , yMin :: a
                   , zMin :: a
                   , xMax :: a
                   , yMax :: a
                   , zMax :: a }

aabbFromCorners (V3 x y z) (V3 x2 y2 z2) = AABB { xMin = min x x2
                                                , yMin = min y y2
                                                , zMin = min z z2
                                                , xMax = max x x2
                                                , yMax = max y y2
                                                , zMax = max z z2 }

minMaxCorners aabb = ( V3 xMin aabb yMin aabb zMin aabb
                     , V3 xMax aabb yMax aabb zMax aabb)

infiniteAABB = AABB { xMin = -infinity, yMin = -infinity, zMin = -infinity
                    , xMax = infinity,  yMax = infinity,  zMax = infinity }

aabbCenter aabb = V3 average (xMax relativeToAABB) (xMin relativeToAABB)
      average (yMax relativeToAABB) (yMin relativeToAABB)
      average (zMax relativeToAABB) (zMin relativeToAABB)

aabbSize aabb = V3 (xMax relativeToAABB - (xMin relativeToAABB))
                   (yMax relativeToAABB - (yMin relativeToAABB))
                   (zMax relativeToAABB - (zMin relativeToAABB))



--data Ray = Ray { origin :: V3 Scalar, direction :: V3 Scalar }

--rayIntersect aabb ray = []

--clamp mn mx = (min mn) . (max mx)

--closestInside aabb closestTo = V3 (clamp xMin xMax (closestTo ^. _X))
