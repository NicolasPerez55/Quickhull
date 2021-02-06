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
    -- Zip every point with the p1-p2 line it needs to be compared to
    p1s = propagateR oldHeadFlags oldPoints
    p2s = propagateL oldHeadFlags oldPoints
    pointsLines = zipWith3 (\pt p1 p2 -> T2 pt (T2 p1 p2)) oldPoints p1s p2s :: Acc (Vector (Point,Line))
    -- Calculate the distance for every point from its line
    distances = map (\(T2 pt line) -> nonNormalizedDistance line pt) pointsLines
    pointsDistances = zipWith T2 oldPoints distances
    -- Find p3 by finding the point with the highest (absolute) distance and propagate it along the entire segment
    p3Step1 = map fst $ segmentedScanl1 (\pA@(T2 _ distA) pB@(T2 _ distB) -> if abs distB > abs distA then pB else pA) oldHeadFlags pointsDistances
    p3s = propagateR (shiftHeadFlagsL oldHeadFlags) p3Step1
    
  in
  T2 oldHeadFlags oldPoints

partitionTesting (T2 oldHeadFlags oldPoints) = let
    p1s = propagateR oldHeadFlags oldPoints
    p2s = propagateL oldHeadFlags oldPoints
    pointsLines = zipWith3 (\pt p1 p2 -> T2 pt (T2 p1 p2)) oldPoints p1s p2s :: Acc (Vector (Point,Line))
    distances = map (\(T2 pt line) -> nonNormalizedDistance line pt) pointsLines
    pointsDistances = zipWith T2 oldPoints distances
    p3Step1 = map fst $ segmentedScanl1 (\pA@(T2 _ distA) pB@(T2 _ distB) -> if abs distB > abs distA then pB else pA) oldHeadFlags pointsDistances
    p3s = propagateR (shiftHeadFlagsL oldHeadFlags) p3Step1
  in p3s

-- The completed algorithm repeatedly partitions the points until there are
-- no undecided points remaining. What remains is the convex hull.
--
quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull pts = init $ asnd $ awhile (\(T2 f p) -> map not (fold1 (&&) f)) partition (initialPartition pts)


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
pointIsRightOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y < c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

{-
pointIsAboveLine :: Exp Line -> Exp Point -> Exp Bool
pointIsAboveLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = y > (c - (nx * x)) / ny
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

pointIsBelowLine :: Exp Line -> Exp Point -> Exp Bool
pointIsBelowLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = y < (c - (nx * x)) / ny
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1
-}

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

segmented :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
segmented f (T2 aF aV) (T2 bF bV) = T2 (aF || bF) (bF ? (bV, f aV bV))

