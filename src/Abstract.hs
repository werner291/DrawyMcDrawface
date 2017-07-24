module Abstract where

import Control.Monad.State.Lazy

data RelativePosition = Above Concept

data Aspect = Above Concept
            | Has Concept
            | Cube
            | Cylinder
            | Sphere deriving (Show)

data Concept = Concept { aspects :: [Aspect] } deriving (Show)

a `above` b = Concept { aspects = Above b : aspects a }
composite :: [Concept] -> Concept
composite components = Concept { aspects = (map Has) components }

primitiveCube = Concept { aspects = [Cube] }

stackOfCubes :: Int -> Concept
stackOfCubes n = composite $ take n $ iterate (\topCube -> primitiveCube `above` topCube) primitiveCube
