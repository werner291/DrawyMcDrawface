
module MonotoneTriangulate where

  compareByXy (V2 x1 y1) (V2 x2 y2)
      | x1 < x2 = LT
      | x1 > x2 = GT
      | y1 < y2 = LT
      | y1 > y2 = GT
      | otherwise = EQ

  newtype MSplitEdge a = (MSplitVertex a, MSplitVertex a)

  yAtX :: (Floating a) -> Edge a -> a
  yAtX atX (vA, vB) = let t = (atX - x vA) / (x vB - x vA)
                        in t * (y vB - y vA) + y vA

  instance (Ord a) => Ord MSplitVertex a where
    compare (MSplitEdge (a,b)) (MSplitEdge (c,d)) =
      let xA = x (vec a)
          xB = x (vec b)
          xC = x (vec c)
          xD = x (vec d)
          cdXMin = min xC xD
          cdXMax = max xC xD
          compareAtX = yAtX (clamp cdXMin cdXMax ((xA + xB) / 2))
          in compare (yAtX (Edge (vert a, vert b))) (yAtX (Edge (vert c, vert d)))

  interiorInsidePolygon v = crossZ (prev v - v) (next v - v) >= 0 -- TODO check if the sign on this is right

  data VertexType = Normal | Split | Merge | Start | End

  vertexType v
     | v > prev v && v > next v
       | interiorInsidePolygon v = Start -- TODO check if the sign on this is right
       | otherwise = Merge -- Mouth to the right
     | v < prev v && v < next v
       | !(interiorInsidePolygon v) = End -- TODO check the sign here
       | otherwise = Split -- Mouth to the left
     | otherwise = Normal

  monotoneSplit :: HalfEdgeDS a vT eT fT -> FacePointer -> (HalfEdgeDS a vT eT fT, [FacePointer])
  monotoneSplit heds face =
        sorted = sortBy (orderByXY) (outerEdges heds face) -- TODO deal with holes (should be easy enough)
        diagonals = evalState (performSweep sorted) (Map.empty, [])
        in splitFace heds face diagonals

  type SweepState = (Map (SplitStateEdge a) (HalfEdgeDS a), [(HalfEdgeDS, HalfEdgeDS)])

  performMonotoneSplitSweep :: [HalfEdgeDS a] -> State SweepState [(HalfEdgeDS, HalfEdgeDS)]

  performMonotoneSplitSweep [] = do
    (_, linesAdded) <- get
    return linesAdded

  performMonotoneSplitSweep (vertex:vertices) = do
    (edgesAndHelpers, _) <- get

    case (vertexType vertex) of
      | Start -> do
        setHelper (outgoingEdge vertex) vertex

      | End -> do
        let oldHelper = (edgesAndHelpers ! (incomingEdge vertex))
        when (vertexType oldHelper == Merge)
          insertDiagonal (Edge oldHelper vertex)
        deleteHelper (incomingEdge vertex)

      | Split ->
        let (edgeBelow, helper) = fromJust $ Map.lookupLE (outgoingEdge vertex)
        insertDiagonal (Edge helper vertex)
        setHelper (edgeBelow vertex) vertex
        setHelper (outgoingEdge vertex) vertex

      | Merge -> do
        let oldHelper = (edgesAndHelpers ! (incomingEdge vertex))
        when (vertexType oldHelper == Merge)
          insertDiagonal (Edge oldHelper vertex)
        deleteHelper (incomingEdge vertex)
        let (edgeBelow, leftOldHelper) = fromJust $ Map.lookupLE (outgoingEdge vertex)
        when (vertexType leftOldHelper == Merge)
          insertDiagonal (Edge leftOldHelper vertex)
        setHelper edgeBelow vertex

      | Normal ->
        if (interiorInsidePolygon vertex)
          then
            when (vertexType oldHelper == Merge)
              insertDiagonal (Edge oldHelper vertex)
            deleteHelper (incomingEdge vertex)
            setHelper (outgoingEdge vertex) vertex
          else
            let (edgeBelow, helper) = fromJust $ Map.lookupLE (outgoingEdge vertex)
            setHelper edgeBelow vertex

    return (performSweep vertices)

  -- Set the helper vertex for a certain edge
  setHelper :: Edge a -> MSplitVertex a -> State (Map (Edge a) (MSplitVertex a), [Edge a])
  setHelper edge helper = do
    (edgesAndHelpers, linesAdded) <- get
    put ( insert edge helper edgesAndHelpers , linesAdded )

  -- Set the helper vertex for a certain edge
  deleteHelper :: Edge a -> MSplitVertex a -> State (Map (Edge a) (MSplitVertex a), [Edge a])
  deleteHelper edge helper = do
    (edgesAndHelpers, linesAdded) <- get
    put ( delete edge edgesAndHelpers , linesAdded )

  insertDiagonal :: Edge a -> State (Map (Edge a) (MSplitVertex a), [Edge])
  insertDiagonal edge = do
    (edgesAndHelpers, linesAdded) <- get
    put (edgesAndHelpers, reverseEdge edge : edge : linesAdded)

  triangulateMonotone :: HalfEdgeDS a vT eT fT -> FacePointer -> (HalfEdgeDS a vT eT fT, [FacePointer])
  triangulateMonotone heds fp = let
    sortedEdges = sortBy compareByXy $ outerEdges heds fp
    triangulateFoldStep (above,below,diagonals) currentEdge =
      if (above == previousId currentEdge)
        then (currentEdge, below, (below, currentEdge) : diagonals)
        else (above, currentEdge, (currentEdge, above) : diagonals)
    (_,_,diagonals) foldl triangulateFoldStep (sortedEdges ! 0, sortedEdges ! 1, []) sortedEdges
    in splitFace heds fp diagonals
