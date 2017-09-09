
module MonotoneTriangulate (triangulate, monotoneSplit, triangulateMonotone) where

  import HalfEdgeDS

  import Data.Map.Lazy (Map)
  import qualified Data.Map.Lazy as Map

  import Control.Arrow (first,second)
  import Control.Monad.State

  import Control.Lens

  import Math (clamp)

  import Data.Maybe

  import Linear

  import Debug.Trace

  import Data.List (sortBy)

  compareByXY :: Ord a
              => HalfEdgeDS a vT eT fT
              -> HalfEdge
              -> HalfEdge
              -> Ordering
  compareByXY heds edgeA edgeB = compare (originCoordinates heds edgeA)
                                         (originCoordinates heds edgeB)

  originCoordinates = liftM2 (.) coordinates origin

  yAtX :: Floating a => a -> (V2 a, V2 a) -> a
  yAtX atX (vA, vB) = let t = (atX - (vA^._x)) / (vB^._x - (vA^._x))
                       in t * (vB^._y - vA^._y) + vA^._y

  interiorInsidePolygon heds v = crossZ
    (originCoordinates heds (previous heds v) - originCoordinates heds v)
    (originCoordinates heds (next heds v) - originCoordinates heds v) >= 0 -- TODO check if the sign on this is right

  data VertexType = Normal | Split | Merge | Start | End deriving (Eq, Show)

  vertexType :: (Ord a, Num a) => HalfEdgeDS a vT eT fT -> HalfEdge -> VertexType
  vertexType heds v
     | compareByXY heds v (previous heds v) == LT
        && compareByXY heds v (next heds v) == LT =
         if interiorInsidePolygon heds v
           then Split -- TODO check if the sign on this is right
           else Start -- Mouth to the right
     | compareByXY heds v (previous heds v) == GT
         && compareByXY heds v (next heds v) == GT =
         if interiorInsidePolygon heds v
           then Merge -- TODO check the sign here
           else End -- Mouth to the left
     | otherwise = Normal

  monotoneSplit :: (Ord a, Num a, Floating a) => HalfEdgeDS a vT eT fT -> Face -> (HalfEdgeDS a vT eT fT, [Face])
  monotoneSplit heds face = let
        sorted = sortBy (compareByXY heds) (fromJust $ outerComponents heds face) -- TODO deal with holes (should be easy enough)
        diagonals = evalState (performMonotoneSplitSweep heds sorted) (Map.empty, [])
        in splitFace face diagonals heds

  type SweepState a vT eT fT = (Map (YOrderedEdge a vT eT fT) HalfEdge, [(HalfEdge, HalfEdge)])

  data YOrderedEdge a vT eT fT = YOrderedEdge (HalfEdgeDS a vT eT fT) HalfEdge

  instance Eq (YOrderedEdge a vT eT fT) where
    (YOrderedEdge _ a) == (YOrderedEdge _ b) = a == b

  instance (Num a, Floating a, Ord a) => Ord (YOrderedEdge a vT eT fT) where
    compare (YOrderedEdge heds edgeA) (YOrderedEdge hedsB edgeB) =
      let va = coordinates heds (origin heds edgeA)
          vb = coordinates heds (target heds edgeA)
          vc = coordinates hedsB (origin heds edgeB)
          vd = coordinates hedsB (target heds edgeB)
          cdXMin = min (vc ^. _x) (vd ^. _x)
          cdXMax = max (vc ^. _x) (vd ^. _x)
          atX = yAtX (clamp cdXMin cdXMax ((va^._x + vb^._x) / 2))
          in compare (atX (va, vb)) (atX (vc,vd))

  performMonotoneSplitSweep :: (Floating a, Num a, Ord a)
                            => HalfEdgeDS a vT eT fT
                            -> [HalfEdge]
                            -> State (SweepState a vT eT fT) [(HalfEdge, HalfEdge)]

  performMonotoneSplitSweep heds [] = do
    (_, linesAdded) <- get
    return linesAdded

  performMonotoneSplitSweep heds (current:halfEdges) = do
    (edgesAndHelpers, _) <- get

    let oldHelper = fromMaybe (error $ "Helper not found for edge: " ++ show current)
                  $ Map.lookup (YOrderedEdge heds $ previous heds current) edgesAndHelpers

    let (YOrderedEdge _ edgeBelow, belowHelper) = fromMaybe (error "Under-edge not found")
                                                $ Map.lookupLE (YOrderedEdge heds current) edgesAndHelpers

    case traceShowId $ vertexType heds current of
      Start -> setHelper heds current current

      End -> do
        when (vertexType heds oldHelper == Merge)
          (insertDiagonal oldHelper current)
        deleteHelper heds $ previous heds current

      Split -> do
        insertDiagonal belowHelper current
        setHelper heds edgeBelow current
        setHelper heds current current

      Merge -> do
        when (vertexType heds oldHelper == Merge)
          (insertDiagonal oldHelper current)
        deleteHelper heds (previous heds current)
        when (vertexType heds belowHelper == Merge)
          (insertDiagonal belowHelper current)
        setHelper heds edgeBelow current

      Normal ->
        if interiorInsidePolygon heds current
          then do
            when (vertexType heds oldHelper == Merge)
              (insertDiagonal oldHelper current)
            deleteHelper heds (previous heds current)
            setHelper heds current current
          else
            setHelper heds edgeBelow current

    performMonotoneSplitSweep heds halfEdges

  -- Set the helper vertex for a certain edge
  setHelper :: (Num a, Ord a, Floating a)
            => HalfEdgeDS a vT eT fT
            -> HalfEdge
            -> HalfEdge
            -> State (SweepState a vT eT fT) ()
  setHelper heds edge helper =
   state (\(map,diags) -> ((), (Map.insert (YOrderedEdge heds edge) helper map , diags)))

  -- Set the helper vertex for a certain edge
  deleteHelper :: (Floating a, Num a, Ord a)
               => HalfEdgeDS a vT eT fT
               -> HalfEdge
               -> State (SweepState a vT eT fT) ()
  deleteHelper heds edge =
    state (\(helpers, diags) -> ((),(Map.delete (YOrderedEdge heds edge) helpers, diags)))

  insertDiagonal :: HalfEdge -> HalfEdge -> State (SweepState a vT eT fT) ()
  insertDiagonal from to =
    state (\(map,list) -> ((),(map,(from, to):list)))

  triangulateMonotone :: (Ord a) => HalfEdgeDS a vT eT fT -> Face -> (HalfEdgeDS a vT eT fT, [Face])
  triangulateMonotone heds fp = let
    sortedEdges = sortBy (compareByXY heds) $ fromMaybe (error "Outer components not found") $ outerComponents heds fp
    triangulateFoldStep (above,below,diagonals) currentEdge =
      if above == previous heds currentEdge
        then (currentEdge, below, (below, currentEdge) : diagonals)
        else (above, currentEdge, (currentEdge, above) : diagonals)
    (_,_,diagonals) = foldl triangulateFoldStep (head sortedEdges, (head . tail) sortedEdges, []) $ init $ drop 2 sortedEdges
    in splitFace fp diagonals heds


  triangulate :: (Ord a, Num a, Floating a)
              => HalfEdgeDS a vT eT fT
              -> Face
              -> (HalfEdgeDS a vT eT fT, [Face])
  triangulate heds face = let
    (hedsMonotone, monotoneFaces) = monotoneSplit heds face
    triangulateFoldStep (heds,triangles) triangle = let
      (newHeds,newTriangles) = triangulateMonotone heds triangle
      in (newHeds,newTriangles ++ triangles) -- TODO check whether this gives O(n^2) running time...
    in foldl triangulateFoldStep (hedsMonotone,[]) monotoneFaces
