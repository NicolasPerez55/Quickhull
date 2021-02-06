{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}

module Quickhull (

  Point, Line, SegmentedPoints,
  quickhull,

  -- Exported for display
  initialPartition,
  partition,

  -- Exported just for testing
  propagateL, shiftHeadFlagsL, segmentedScanl1,
  propagateR, shiftHeadFlagsR, segmentedScanr1,

) where

import Data.Array.Accelerate
import qualified Prelude                      as P


-- Points and lines in two-dimensional space
--
type Point = (Int, Int)
type Line  = (Point, Point)

-- This algorithm will use a head-flags array to distinguish the different
-- sections of the hull (the two arrays are always the same length).
--
-- A flag value of 'True' indicates that the corresponding point is
-- definitely on the convex hull. The points after the 'True' flag until
-- the next 'True' flag correspond to the points in the same segment, and
-- where the algorithm has not yet decided whether or not those points are
-- on the convex hull.
--
type SegmentedPoints = (Vector Bool, Vector Point)


-- Core implementation
-- -------------------

-- Initialise the algorithm by first partitioning the array into two
-- segments. Locate the left-most (p₁) and right-most (p₂) points. The
-- segment descriptor then consists of the point p₁, followed by all the
-- points above the line (p₁,p₂), followed by the point p₂, and finally all
-- of the points below the line (p₁,p₂).
--
-- To make the rest of the algorithm a bit easier, the point p₁ is again
-- placed at the end of the array.
--
-- We indicate some intermediate values that you might find beneficial to
-- compute.
--
initialPartition :: Acc (Vector Point) -> Acc SegmentedPoints
initialPartition points =
  let
      -- Get the left-bottom-most and right-top-most points by getting the minimum and maximum point respectively
      -- (the bottom left point should always have a lower x coordinate, and optionally a lower y coordinate)
      p1, p2 :: Exp Point
      p1 = the $ minimum points
      p2 = the $ maximum points
      line = T2 p1 p2

      -- Create an array of bools that has True for all points "above" (left of) the line
      isUpper :: Acc (Vector Bool)
      isUpper = map (pointIsLeftOfLine line) points

      -- Create an array of bools that has True for all points "below" (right of) the line
      isLower :: Acc (Vector Bool)
      isLower = map (pointIsRightOfLine line) points

      -- Generate an index array: every value is its own index number
      indices = generate (shape points) (\(I1 i) -> i)

      -- Get the indices of the points above the line
      offsetUpper :: Acc (Vector Int)
      countUpper  :: Acc (Scalar Int)
      T2 offsetUpper countUpper = compact isUpper indices

      -- Get the indices of the points below the line
      offsetLower :: Acc (Vector Int)
      countLower  :: Acc (Scalar Int)
      T2 offsetLower countLower = compact isLower indices

      -- Create a set of target indices for both the points above and below the line
      upperIndices = generate (I1 (the countUpper)) (\(I1 i) -> Just_ (I1 (i + 1)))
      lowerIndices = generate (I1 (the countLower)) (\(I1 i) -> Just_ (I1 (i + 2 + the countUpper)))

      -- Assign destinations to all points above the line, leaving empty the spots between them
      preDestination :: Acc (Vector (Maybe DIM1))
      preDestination = scatter offsetUpper (fill (shape points) Nothing_) (upperIndices)

      -- Assign destinations to all points below the line as well.
      -- Any point not below or above keeps a Nothing for its destination, and will thus not be kept.
      destination :: Acc (Vector (Maybe DIM1))
      destination = scatter offsetLower preDestination (lowerIndices)

      p2Pos = the countUpper + 1
      finalLength = 3 + the countUpper + the countLower
      justTheLine = generate (I1 finalLength) (\(I1 i) -> if i==0 || i == finalLength-1 then p1 else if i == p2Pos then p2 else T2 0 0)

      -- Put the old points in their new destination spot
      newPoints :: Acc (Vector Point)
      newPoints = permute const justTheLine (\ix -> destination!ix) points

      headFlags :: Acc (Vector Bool)
      headFlags = generate (I1 finalLength) (\(I1 i) -> if i==0 || i==p2Pos || i == finalLength-1 then True_ else False_)


  in
  T2 headFlags newPoints


-- The core of the algorithm processes all line segments at once in
-- data-parallel. This is similar to the previous partitioning step, except
-- now we are processing many segments at once.
--
-- For each line segment (p₁,p₂) locate the point furthest from that line
-- p₃. This point is on the convex hull. Then determine whether each point
-- p in that segment lies to the left of (p₁,p₃) or the right of (p₂,p₃).
-- These points are undecided.
--
partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 oldHeadFlags oldPoints) = let
    -- Calculate the distance for every point from its line
    p1s = propagateL oldHeadFlags oldPoints
    p2s = propagateR oldHeadFlags oldPoints
    distances = zipWith3 (\pt p1 p2 -> nonNormalizedDistance (T2 p1 p2) pt) oldPoints p1s p2s
    pointsDistances = zipWith T2 oldPoints distances

    -- Find p3 by finding the point with the highest distance and propagate it along the entire segment
    p3Step1 = map fst $ segmentedScanl1 (\pA@(T2 _ distA) pB@(T2 _ distB) -> if distB > distA then pB else pA) oldHeadFlags pointsDistances
    p3s = propagateR (shiftHeadFlagsL oldHeadFlags) p3Step1

    -- Find which points are left of p1-p3
    isLeftOfTriangle :: Acc (Vector Bool)
    isLeftOfTriangle = zipWith3 (\pt p1 p3 -> pointIsLeftOfLine (T2 p1 p3) pt) oldPoints p1s p3s

    -- Find which points are right of p3-p2
    isRightOfTriangle :: Acc (Vector Bool)
    isRightOfTriangle = zipWith3 (\pt p3 p2 -> pointIsLeftOfLine (T2 p3 p2) pt) oldPoints p3s p2s
  in
  T2 oldHeadFlags oldPoints

partitionTesting (T2 oldHeadFlags oldPoints) = let
    -- Calculate the distance for every point from its line
    p1s = propagateL oldHeadFlags oldPoints
    p2s = propagateR oldHeadFlags oldPoints
    distances = zipWith3 (\pt p1 p2 -> nonNormalizedDistance (T2 p1 p2) pt) oldPoints p1s p2s
    pointsDistances = zipWith T2 oldPoints distances

    -- Find p3 by finding the point with the highest distance and propagate it along the entire segment
    p3Step1 = map fst $ segmentedScanl1 (\pA@(T2 _ distA) pB@(T2 _ distB) -> if distB > distA then pB else pA) oldHeadFlags pointsDistances
    p3s = propagateR (shiftHeadFlagsL oldHeadFlags) p3Step1

    -- Find which points are left of p1-p3
    isLeftOfTriangle :: Acc (Vector Bool)
    isLeftOfTriangle = zipWith3 (\pt p1 p3 -> pointIsLeftOfLine (T2 p1 p3) pt) oldPoints p1s p3s

    -- Find which points are right of p3-p2
    isRightOfTriangle :: Acc (Vector Bool)
    isRightOfTriangle = zipWith3 (\pt p3 p2 -> pointIsLeftOfLine (T2 p3 p2) pt) oldPoints p3s p2s

    -- Find the indices where every segment starts, and what spot everything is in their segment
    indicesSegments = propagateL oldHeadFlags $ generate (shape oldPoints) (\(I1 i) -> i) :: Acc (Vector Int)
    indicesSegmented = segmentedScanl1 (+) oldHeadFlags (fill (shape oldPoints) 1) :: Acc (Vector Int)
    indices = zipWith T2 indicesSegments indicesSegmented

    -- Get the indices of the points left of the triangles
    offsetLeft :: Acc (Vector (Int, Int))
    countLeftTotal  :: Acc (Scalar Int)
    T2 offsetLeft countLeftTotal = compact isLeftOfTriangle indices

    -- Get the indices of the points below the line
    offsetRight :: Acc (Vector (Int, Int))
    countRightTotal  :: Acc (Scalar Int)
    T2 offsetRight countRightTotal = compact isRightOfTriangle indices

    countLeft = propagateR (shiftHeadFlagsL oldHeadFlags) $ segmentedScanl1 (+) oldHeadFlags (boolsToInts isLeftOfTriangle)
    countRight = propagateR (shiftHeadFlagsL oldHeadFlags) $ segmentedScanl1 (+) oldHeadFlags (boolsToInts isRightOfTriangle)

    

  in countRight

boolsToInts :: Acc (Vector Bool) -> Acc (Vector Int)
boolsToInts = map (\bool -> if bool then 1 else 0)

-- The completed algorithm repeatedly partitions the points until there are
-- no undecided points remaining. What remains is the convex hull.
--
-- Start with initialPartition.
-- As long as there is at least one False value in our headflags array, keep applying partition to reduce.
-- Once finished, take the second part of the SegmentedPoints (the points themselves).
-- Now take everything but the last point (the duplicate of p1, and we have our hull.
quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull = init . asnd . (awhile (map not . fold1 (&&) . afst) partition) . initialPartition


-- Helper functions
-- ----------------

propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedScanl1 const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedScanr1 (P.flip const)

shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL vec' = permute const (fill (shape vec') True_) (\(I1 i) -> Just_ (I1 (i-1))) vec'

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR vec' = permute const (fill (shape vec') True_) (\(I1 i) -> Just_ (I1 (i+1))) vec'

-- Code for segmentedScanl1 and segmentedScanr1 adapted from the accelerate library functions scanl1Seg and scanr1Seg
segmentedScanl1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl1 f flags' vec' =  map snd
                                  . scanl1 (segmented f)
                                  $ zip (replicate (lift (indexTail (shape vec') :. All)) flags') vec'

segmentedScanr1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanr1 f flags' vec' =  map snd
                                  . scanr1 (\x y -> segmented (P.flip f) y x)
                                  $ zip (replicate (lift (indexTail (shape vec') :. All)) flags') vec'


-- Given utility functions
-- -----------------------

pointIsLeftOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsLeftOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y > c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

pointIsRightOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsRightOfLine (T2 a b) c = pointIsLeftOfLine (T2 b a) c

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

segmented :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
segmented f (T2 aF aV) (T2 bF bV) = T2 (aF || bF) (bF ? (bV, f aV bV))

