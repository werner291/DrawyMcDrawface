
module HalfEdgeDS ( next
                  , previous
                  , origin
                  , target
                  , outerComponent
                  , outerComponents
                  , fromPolygon
                  , coordinates
                  , splitFace
                  , listVertices
                  , listEdges
                  , listFaces
                  , prop_twinOfTwin
                  , prop_previousOfNext
                  , prop_nextOfPrevious
                  , prop_fromPolygonMakesCorrectHEDS
                  , prop_correctFacePointers
                  , prop_hedsOk
                  , HalfEdgeDS
                  , HalfEdge
                  , Vertex
                  , Face) where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Maybe (fromMaybe, fromJust)

import Control.Monad (liftM2)

import Debug.Trace

import Linear

type Vertex = Int
type HalfEdge = Int
type Face = Int

data VertexData a vT = VertexData { _coordinates :: V2 a
                                  , _userData :: vT
                                  , _incidentEdgeId :: HalfEdge } deriving (Show)

data HalfEdgeData a eT = HalfEdgeData { _origin :: Vertex
                                      , _twin :: HalfEdge
                                      , _incidentFace :: Face
                                      , _next :: HalfEdge
                                      , _prev :: HalfEdge } deriving (Show)

data FaceData a fT = FaceData { _outerComponent :: Maybe Int
                              , _innerComponents :: IntSet } deriving (Show)

data HalfEdgeDS a vT eT fT = HalfEdgeDS { vertices :: IntMap (VertexData a vT)
                                        , edges :: IntMap (HalfEdgeData a eT)
                                        , faces :: IntMap (FaceData a fT) } deriving (Show)

vertexData :: HalfEdgeDS a vT eT fT -> Vertex -> VertexData a vT
vertexData heds vertex = fromMaybe (error ("No vertex with ID: " ++ show vertex))
                       $ IntMap.lookup vertex (vertices heds)

coordinates :: HalfEdgeDS a vT eT fT -> Vertex -> V2 a
coordinates = (_coordinates .) . vertexData

-- | (Internal) Retrieve the edge data for a given node in a HEDS
edgeData :: HalfEdgeDS a vT eT fT -> HalfEdge -> HalfEdgeData a eT
edgeData heds he = fromMaybe (error ("No halfedge with ID: " ++ show he))
                       $ IntMap.lookup he (edges heds)

faceData :: HalfEdgeDS a vT eT fT -> HalfEdge -> FaceData a fT
faceData heds face = fromMaybe (error ("No face with ID: " ++ show face))
                       $ IntMap.lookup face (faces heds)

-- | Retrieve the origin Vertex of the halfedge
origin :: HalfEdgeDS a vT eT fT -> HalfEdge -> Vertex
origin = (_origin .) . edgeData

-- | Retrieve the origin Vertex of the halfedge
target :: HalfEdgeDS a vT eT fT -> HalfEdge -> Vertex
target = liftM2 (.) origin twin

-- | Retrieve the next halfedge of the halfedge in the HEDS
next :: HalfEdgeDS a vT eT fT -> HalfEdge -> HalfEdge
next = (_next .) . edgeData

-- Retireve the previous halfedge in a HEDS
previous :: HalfEdgeDS a vT eT fT -> HalfEdge -> HalfEdge
previous = (_prev .) . edgeData

-- Retireve the previous halfedge in a HEDS
twin :: HalfEdgeDS a vT eT fT -> HalfEdge -> HalfEdge
twin = (_twin .) . edgeData

incidentFace :: HalfEdgeDS a vT eT fT -> HalfEdge -> Face
incidentFace = (_incidentFace .) . edgeData

exploreEdges :: HalfEdgeDS a vT eT fT -> HalfEdge -> [HalfEdge]
exploreEdges = iterate . next

exploreEdgesOnce :: HalfEdgeDS a vT eT fT -> HalfEdge -> [HalfEdge]
exploreEdgesOnce heds he = let looping = exploreEdges heds he
                             in he : takeWhile (/= he) (tail looping)

outerComponent :: HalfEdgeDS a vT eT fT -> Face -> Maybe HalfEdge
outerComponent = (_outerComponent .) . faceData

outerComponents :: HalfEdgeDS a vT eT fT -> Face -> Maybe [HalfEdge]
outerComponents heds face = exploreEdgesOnce heds <$> outerComponent heds face

listEdges :: HalfEdgeDS a vT eT fT -> [HalfEdge]
listEdges = IntMap.keys . edges
listVertices = IntMap.keys . vertices
listFaces = IntMap.keys . faces

newVertex :: HalfEdgeDS a vT eT fT -> Vertex
newVertex heds = if (null.vertices) heds
                      then 0
                      else succ $ (fst.(IntMap.findMax).vertices) heds

newVertices heds = enumFrom.newVertex

newEdge heds = if (null.edges) heds
                      then 0
                      else succ $ (fst.(IntMap.findMax).edges) heds

newEdges = enumFrom.newEdge

newFace heds = if (null.faces) heds
                      then 0
                      else succ $ (fst.(IntMap.findMax).faces) heds

newFaces = enumFrom.newFace

-- Invariants and properties --

-- Mutual twinship
prop_twinOfTwin heds edge = edge == twin heds (twin heds edge)

-- Prev-next correctness
prop_previousOfNext heds edge = edge == previous heds (next heds edge)
prop_nextOfPrevious heds edge = edge == next heds (previous heds edge)

listMaybeToList :: Maybe [a] -> [a]
listMaybeToList (Just xs) = xs
listMaybeToList Nothing = []

prop_correctFacePointers heds face =
 all (== face) $ incidentFace heds <$> listMaybeToList (outerComponents heds face)

prop_allEdgesInFace heds edge =
 and $ (\edge -> edge `elem` (exploreEdgesOnce heds . fromJust . outerComponent heds . incidentFace heds) edge) <$> listEdges heds

prop_hedsOk heds = let
  edgesOk =  all (prop_twinOfTwin heds) (listEdges heds)
          && all (prop_previousOfNext heds) (listEdges heds)
          && all (prop_nextOfPrevious heds) (listEdges heds)
  facesOk = all (prop_correctFacePointers heds) (listFaces heds)
  in edgesOk && facesOk

-- Operations --

-- | splitFace Splits a face into multiple subfaces along a certain set of edges
splitFace :: Face -- ^ Pointer to the face inside the HEDS (which must exist)
          -> [(HalfEdge,HalfEdge)]
          -- ^ List of pairs of Half-edge pointers.
          -- The face will be split with edges drawn between the origins of these half edges
          -- These edges must exist and lie on the boundary of the face.
          -- Duplicates or twins may appear in this list, bu this is not necessary
          -> HalfEdgeDS a vT eT fT -- ^ The HEDS in which the face lives
          -> (HalfEdgeDS a vT eT fT, [Face])
          -- ^ A pair containing the new HEDS containing the split face, and a list of
          -- FacePointers that refer to the newly-formed faces.
splitFace face toInsert oldHeds = let

  unsafe_insertEdgeBeforeEdges (heds,insertedEdges) (a, b) =
    let (newEdgeId : newTwinId : _) = newEdges heds
        edgeExists = (previous heds . previous heds) a == b -- Check duplicates
      -- We know this is not null since we start out with a face
        edgesWithDiagonal =
                   IntMap.adjust (\e -> e { _prev = newTwinId }) a
                 $ IntMap.adjust (\e -> e { _next = newEdgeId }) (previous heds a)
                 $ IntMap.insert newEdgeId HalfEdgeData { _origin = origin heds a
                                                        , _twin = newTwinId
                                                        , _incidentFace = undefined -- ?
                                                        , _next = b
                                                        , _prev = a }
                 $ IntMap.adjust (\e -> e { _prev = newEdgeId }) b
                 $ IntMap.adjust (\e -> e { _next = newTwinId }) (previous heds b)
                 $ IntMap.insert newTwinId HalfEdgeData { _origin = origin heds a
                                                        , _twin = newEdgeId
                                                        , _incidentFace = undefined -- ?
                                                        , _next = a
                                                        , _prev = b }
                 $ edges heds

      in if edgeExists
           then (heds, insertedEdges)
           else (heds { edges = edgesWithDiagonal } , newEdgeId:newTwinId:insertedEdges)

  -- Fold over toInsert to insert the new edges, their twins, and adjust the previous/next pointers
  (newHedsBrokenFaces, insertedEdges) =
      foldl unsafe_insertEdgeBeforeEdges (oldHeds,[]) toInsert

  -- List of all half-edges that need to change in this operation
  allAffectedEdges = traceShow insertedEdges $ insertedEdges ++ fromMaybe (error "Splitting unbounded face...") (outerComponents oldHeds face)

  makeFaces :: HalfEdgeDS a vT eT fT -> [HalfEdge] -> (HalfEdgeDS a vT eT fT, IntMap Face, [Face])
  makeFaces heds (edge:todo) = let
    (rHeds, edgesToFace, facesSoFar) = makeFaces heds todo
    face = newFace rHeds
    faceData = FaceData { _outerComponent = Just edge
                        , _innerComponents = IntSet.empty }
    setIncidentFace = IntMap.adjust (\e -> e { _incidentFace = newFace rHeds })
    in if IntMap.member edge edgesToFace
         then (rHeds, edgesToFace, facesSoFar)
         else ( rHeds { edges = foldl (flip setIncidentFace) (edges rHeds) (exploreEdgesOnce rHeds edge)
                      , faces = IntMap.insert face faceData (faces rHeds) }
              , foldl (\acc edge -> IntMap.insert edge face acc) edgesToFace (exploreEdgesOnce rHeds edge)
              , face:facesSoFar )
  makeFaces heds [] = (heds {faces = IntMap.delete face $ faces heds }, IntMap.empty, [])

  (newHeds, _, newFaces) = makeFaces newHedsBrokenFaces allAffectedEdges
   -- Start with no edges processed, the original heds and an empty list of created faces

  -- Top-level call to makeFaces
  in (newHeds, newFaces)

fromPolygon :: [V2 a] -> (HalfEdgeDS a () () (), Face)
fromPolygon verts = let
  numVerts = length verts
  interiorFaceId = 0 -- Id of the face inside the polygon
  unboundedFaceId = 1 -- Id of the unbounded face
  in ( HalfEdgeDS { vertices = IntMap.fromList $ do
                     -- Make a HEDSVertex for each vertex, in the same order, 0-indexed
                      (i,v) <- zip [0..] verts
                      return (i, VertexData { _coordinates = v
                                            , _userData = ()
                                            , _incidentEdgeId = 0 } )
                  , edges = IntMap.fromList $ concat $
                      -- List of vertex IDs in the same order as the polygon verts
                      (\i ->
                      [ ( i * 2 -- Even numbers of the interior
                      -- Generate interleaved of interior-exterior edges

                             , HalfEdgeData { _origin = i
                                            , _twin = i * 2 + 1 -- Twin is the one jst after
                                            , _incidentFace = interiorFaceId
                                            , _next = ((i+1) `mod` numVerts)*2 -- next even number (modulo)
                                            , _prev = ((i-1) `mod` numVerts)*2 } ) -- previous even number (modulo) )
                      , ( i * 2 + 1 -- Odd numbers for the unbounded face
                             , HalfEdgeData { _origin = (i + 1) `mod` numVerts
                                            , _twin = i * 2 -- Just before
                                            , _incidentFace = unboundedFaceId
                                            , _next = ((i+1)*2 + 1) `mod` (numVerts *2) -- next odd number
                                            , _prev = ((i-1)*2 + 1) `mod` (numVerts *2) } ) ] -- previous odd number
                      )<$> [0..(numVerts - 1)]
                  , faces = IntMap.fromList [ ( interiorFaceId -- Interior
                                            , FaceData { _outerComponent = Just 0 -- The first inner edge
                                                       , _innerComponents = IntSet.empty } ) -- No holes
                                          , ( unboundedFaceId
                                            , FaceData { _outerComponent = Nothing -- Boundless face
                                                       , _innerComponents = IntSet.singleton 1 } ) ] } -- One hole: the polygon
     , interiorFaceId)

prop_fromPolygonMakesCorrectHEDS polygon = prop_hedsOk (fst $ fromPolygon polygon)