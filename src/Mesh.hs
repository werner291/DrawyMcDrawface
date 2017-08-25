
module Mesh where

import Linear

import Control.Monad (liftM)

data Triangle a = Triangle (V3 a) (V3 a) (V3 a) deriving (Show)

newtype PolyLine a = PolyLine [V3 a]

data Face a = Face [V3 a] deriving (Show)
vertices :: Face a -> [V3 a]
vertices (Face verts) = verts

data Mesh a = Mesh [Face a] deriving (Show)

faces (Mesh fs) = fs

mergeAll :: [Mesh a] -> Mesh a
mergeAll meshes = Mesh (concat (map faces meshes))

merge :: Mesh a -> Mesh a -> Mesh a
merge (Mesh a) (Mesh b) = Mesh (a++b)

translate :: (Num a) => Mesh a -> V3 a -> Mesh a
translate (Mesh faces) vec = Mesh $ fmap (flip translateFace vec) faces

translateFace :: (Num a) => Face a -> V3 a -> Face a
translateFace (Face verts) trans = Face (map (trans ^+^) verts)

vertexMap fun (Mesh faces) = Mesh (map (vertexMapFace fun) faces)
vertexMapFace fun (Face verts) = Face (map fun verts)

triangulateFanFromFirst :: Face a -> [Triangle a]
triangulateFanFromFirst (Face verts) =
  map (\(b,c) -> Triangle (head verts) b c ) (pairwise (drop 1 verts))

regularNgonVertices :: (Integral n, Floating r) => n -> r -> [V3 r]
regularNgonVertices sides radius = map (\t -> V3 (radius * cos t) (radius * sin t) 0.0)
                                 $ map (\i -> fromIntegral i * 2.0 * (pi / fromIntegral sides)) [0..sides]

regularNgon :: (Integral n, Floating r) => n -> r -> Face r
regularNgon sides radius = Face $ regularNgonVertices sides radius

extrudeToFaces :: (Floating a) => PolyLine a -> V3 a -> [Face a]
extrudeToFaces (PolyLine verts) dir = let opposingVerts = zip verts (( ^+^ dir) <$> verts)
                                      in (\((a,b),(c,d))-> Face [a,b,d,c]) <$> pairwise opposingVerts

extrude :: (Floating a) => Face a -> V3 a -> Mesh a
extrude = extrudeSegmented 1

extrudeSegmented :: (Floating a) => Int -> Face a -> V3 a -> Mesh a
extrudeSegmented segs face dir =
  let is = [0..segs]
      ts = (\i -> fromIntegral i/(fromIntegral segs)) <$> is
      faceStack = (\t -> translateFace face (dir * t)) <$> ts
      rings = vertices <$> faceStack
      ringPairs = pairwise rings
      opposingEdgePairs = concat $ (\(ringA, ringB) -> pairwise $ addHeadToEnd $ zip ringA ringB) <$> ringPairs
      sideFaces = (\((a,b),(c,d)) -> Face [a,b,d,c]) <$> opposingEdgePairs
  in Mesh (face : (translateFace face dir) : sideFaces)

square = Face [V3 (-0.5) (-0.5) 0.0, V3 0.5 (-0.5) 0.0, V3 0.5 0.5 0.0, V3 (-0.5) 0.5 0.0]

cube = extrude square (V3 0.0 0.0 2.0) :: Mesh Float
