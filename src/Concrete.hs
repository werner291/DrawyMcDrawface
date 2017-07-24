data ConcShape = PrimCube | PrimCylinder deriving Show

-- Concrete

type Position = V3 Float

data Drawable = Drawable {position :: Position,
                          shape :: ConcShape,
                          children :: [Drawable]} deriving (Show)

up = V3 0 0 1

drawableFromConcept :: Concept -> Drawable
drawableFromConcept c =
  foldr (flip applyAspect) (Drawable {position=V3 0 0 0, shape = PrimCube, children = []}) (aspects c)

drawableFromConcept c = applyAspect (drawableFromConcept conceptWithoutAspect) (head (aspects c))
    where conceptWithoutAspect = c{aspects = drop 1 (aspects c)}


applyAspect :: Drawable -> Aspect -> Drawable
applyAspect original aspect = case aspect of
  Above relativeTo -> let otherPos = position $ drawableFromConcept relativeTo
                      in original {position = otherPos + up * 2}
  Has childConcept -> original {children = drawableFromConcept childConcept : children original}
  _ -> original -- Ignore the others for now

flattenDrawable :: Drawable -> [Drawable]
flattenDrawable drawable = drawable : (concat $ map flattenDrawable (children drawable))
