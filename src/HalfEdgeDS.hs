
module HalfEdgeDS where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

type VertexPointer = Int
type HalfEdgePointer = Int
type FacePointer = Int

data HEDSVertex a vT = HEDSVertex { coordinates :: V2 a
                                  , userData :: vT,
                                  , incidentEdgeId :: HalfEdgePointer } deriving (Eq)

data HEDSHalfEdge a eT = HEDSHalfEdge { originId :: VertexPointer
                                      , twinId :: HalfEdgePointer
                                      , incidentFaceId :: FacePointer
                                      , nextId :: HalfEdgePointer
                                      , prevId :: HalfEdgePointer } deriving (Eq)

edgeId = prevId . nextId

next heds edge = (edges heds) ! (nextId edge)
previous heds edge = (edges heds) ! (prevId edge)

exploreEdges :: HalfEdgeDS a vT eT fT -> HEDSHalfEdge a eT -> [HEDSHalfEdge a eT]
exploreEdges heds edge = edge : takeWhile (\e -> edgeId edge != edgeId e) (iterate (next heds) edge)

data HEDSFace a fT = HEDSFace { outerComponentId :: Maybe Int
                              , innerComponentIds :: IntSet } deriving (Eq)

outerComponent heds face = (faces heds) ! (outerComponentId face)

data HalfEdgeDS a vT eT fT = HalfEdgeDS { vertices :: IntMap (PSVertex a vT)
                                        , edges :: IntMap (PSHalfEdge a eT)
                                        , faces :: IntMap (PSFace a fT),
                                        , nextVertexId :: Int
                                        , nextEdgeId :: Int
                                        , nextFaceId :: Int }

outerEdges heds face = exploreEdges heds (incidentEdge heds face)

-- | splitFace Splits a face into multiple subfaces along a certain set of edges
splitFace :: HalfEdgeDS a vT eT fT -- ^ The HEDS in which the face lives
          -> FacePointer -- ^ Pointer to the face inside the HEDS (which must exist)
          -> [(HalfEdgePointer,HalfEdgePointer)]
          -- ^ List of pairs of Half-edge pointers.
          -- The face will be split with edges drawn between the origins of these half edges
          -- These edges must exist and lie on the boundary of the face.
          -- Duplicates or twins may appear in this list, bu this is not necessary
          -> (HalfEdgeDS a vT eT fT, [FacePointer])
          -- ^ A pair containing the new HEDS containing the split face, and a list of
          -- FacePointers tha trefer to the newly-formed faces.
splitFace heds face toInsert = let
  -- 2-by-2
  newEdgeIds = sliding 2 2 [nextEdgeId heds ..]
  faceBoundary = outerEdges heds face
  -- Map of EdgePointers to edges, essentially represents the changes to be made to the edges of heds
  diagonals = IntMap.fromList $ concat ((\((a, b) , [newEdgeId, newTwinId]) ->
      [ (a, edges heds ! a { prevId = newTwinId })
      , (prevId $ edges heds ! a, (previous heds $ prevId $ edges heds ! a))
         -- previous of a, now pointing to diagonal as next
      , (newEdgeId, (edges heds ! a) {nextId = b, twinId = newTwinId} )
         -- diagonal, points to b as next, previous need not be changed
      , (b, edges heds ! b { prevId = newEdgeId })
      , (prevId $ edges heds ! b, (previous heds $ prevId $ edges heds ! b))
         -- previous of b, now pointing to diagonal twin as next
      , (newTwinId, (edges heds ! b) {nextId = a, twinId = newEdgeId} ) ])
         -- diagonal, points to a as next
    <$> toInsert)

  -- IntMap of edges, with their EdgePointer as ID
  faceBoundaryEdges = IntMap.fromList $ zip (edgeId <$> faceBoundary) faceBoundary

  -- Combine the two maps of edges, using the left-hand value when available
  totalNewEdges = IntMap.union diagonals faceBoundaryEdges
    -- Set of edhes with their "next"
    -- Left-biased, so new connections from diagonals take precedence
    -- WARNING: the incidentFaceId may be incorrect on some of these.

  makeFaces accHeds nextMap faces
    -- Function that takes a HEDS and an IntMap of half-edges
    -- and returns a list of faces and a HEDS having those faces
    -- by analysing the loops that appear in the edge map
    | null nextMap = (accHeds , faces) -- Base case, just return the HEDS unmodified,
                                       -- and the list of face pointes
    | otherwise = let
      -- Take the edge with the lowest ID that is still in the IntMap
      firstEdgeId = snd (IntMap.findMin nextMap)
      -- Make a list of edge pointers by following the nextId repeatedly.
      faceEdgeIds = edgeId <$> exploreEdges $ snd (IntMap.findMin nextMap)
      -- Function that changed the incidentFaceId to the newFaceId of the HEDS
      setIncidentFace = \edge -> edge {incidentFaceId = newFaceId accHeds}
      -- Create a new face with the firstEdgeId as its' outer component
      newFace = HEDSFace { outerComponentId = Just firstEdgeId
                         , innerComponentIds = IntSet.empty }
      -- Create a new HEDS that has the incidentFaceId set for this new face.
      newAcc = accHeds { edges = foldl (IntMap.adjust setIncidentFace) (edges accHeds) faceEdgeIds
                         -- Adjust the incident face of the edges in the boundary
                       , faces = IntMap.insert (newFaceId accHeds) newFace (faces accHeds)
                         -- Insert the new face in the faces
                       , nextFaceId = succ $ nextFaceId accHeds }
                         -- Update the nextFaceId

      -- Delete all the edges from the edge Map that we encountered so far
      newNextMap = foldl IntMap.delete nextMap faceEdgeIds

      -- Recurse with the remaining edges
      in makeFaces newAxx newNextMap (newFaceId accHeds : faces)

  -- Top-level call to makeFaces
  in makeFaces (heds {IntMap.delete face -- Delete the face that was destroyed by splitting
                     -- Advance the nextEdgeId by the number of half-edges we just created
                     , nextEdgeId = nextEdgeId heds + length toInsert * 2})
               totalNewEdges -- Start with the edge map with all the outer-border edges of the face
               [] -- Empty list of created faces
