{-# LANGUAGE FlexibleContexts #-}

module Building where

import Mesh
import Math
import Linear
import Path

import Control.Monad.Random
import Control.Monad.Supply
import Data.Maybe

import HalfEdgeDS
import MonotoneTriangulate

{-seed = 1337

curve = CubicBezierCurve (V3 0 0 0) (V3 10 0 0) (V3 (50) 0 10) (V3 60 10 0)

data Room a = Room { roomId :: Int, floorShape :: MultiPolygon a }

newtype DirectedEdge a = DirectedEdge (V2 a, V2 a)

makeRoom xMin xMax yMin yMax roomId =
  Room { roomId = roomId
       , floorShape = MultiPolygon [ Polygon [ V2 xMin yMin
                                             , V2 xMax yMin
                                             , V2 xMax yMax
                                             , V2 xMin yMax ] ] }

data Floorplan a = Floorplan { rooms :: [Room a] }

-- Complexity: O(n).
choice :: (MonadRandom m) => [a] -> m a
choice [] = error "Control.Monad.Random.Extras.choice: empty list"
choice xs = (xs !!) `liftM` getRandomR (0, length xs - 1)

makeFloorplan :: (MonadRandom m, MonadSupply Int m) => Int -> m (Floorplan Float)
makeFloorplan 1 = do
  width <- getRandomR (2,5)
  height <- getRandomR (2,5)
  roomId <- supply
  return Floorplan { rooms = [makeRoom (-width / 2) (width / 2) (-height / 2) (height / 2) roomId] }

makeFloorplan n = do
  subFloorPlan <- makeFloorplan (n-1)
  selectedRoomShape <- choice (floorShape <$> (rooms subFloorPlan))
  (Edge (edgeA, edgeB)) <- choice (multiPolygonEdges selectedRoomShape)
  let expandDir = perp $ edgeA ^-^ edgeB
  expandLength <- getRandomR (2,5)
  roomId <- supply
  let roomShape = Polygon [ edgeA
                          , edgeB
                          , edgeB ^+^ expandDir ^* expandLength
                          , edgeA ^+^ expandDir ^* expandLength ]
  return (subFloorPlan { rooms = Room { roomId = roomId
                                      , floorShape = MultiPolygon [roomShape] } : rooms subFloorPlan })

building :: (MonadRandom m, MonadSupply Int m) => m (Mesh Float)
building = do
  floorPlan <- makeFloorplan 2
  let faces = Face <$> polygonVerts <$> floorShape <$> rooms floorPlan
  let floors = (flip extrude (V3 0 0 1)) <$> faces
  return $ mergeAll floors -}

building :: Mesh Float
building = let
  (heds, polygon) = fromPolygon [V2 0 0, V2 (-5) (-2), V2 10 0, V2 (-5) 2]
  (hedsTri, triangles) = triangulate heds polygon
  make3D (V2 x y) = V3 x y 0
  trianglesByEdges = (fromJust . outerComponents hedsTri <$> triangles) :: [[HalfEdge]]
  trianglesByVertices = (\tri -> (make3D . coordinates hedsTri) . origin hedsTri <$> tri) <$> trianglesByEdges

  faces = Face <$> trianglesByVertices
  in mergeAll $ flip extrude (V3 0 0 1) <$> faces
