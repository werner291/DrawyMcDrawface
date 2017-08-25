
module Path where

import Linear

data BezierCurve a = CubicBezierCurve (V3 a) (V3 a) (V3 a) (V3 a) deriving (Show)

pointOnCurve :: (Floating t) => BezierCurve t -> t -> V3 t
pointOnCurve (CubicBezierCurve a eA eB b) t = t**3 *^ b
                                            + 3 * t**2 * (1-t) *^ eB
                                            + 3 * t * (1-t)**2 *^ eA
                                            + (1-t)**3 *^ a

tangentOnCurve :: (Floating t) => BezierCurve t -> t -> V3 t
tangentOnCurve (CubicBezierCurve a eA eB b) t = 3 * t**2 *^ (a - eA)
                                              + 6 * t * (1-t) *^ (eA - eB)
                                              + 3 * (t-1)**2 *^ (eB - b)
