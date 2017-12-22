{-
  File      :  CollisionDetection.hs
  Name :        Juan Arroyo Miranda, 10/04/17
  Contains types for representing 2D objects and collision functions for them.
-}

-- Define Point type as a tuple of Doubles
type Point = (Double, Double)
-- Define LineSegment type as a tuple of Points
type LineSegment = (Point, Point)
-- Define BoundingBox type as a tuple of LineSegments
type BoundingBox = (LineSegment, LineSegment, LineSegment, LineSegment)
-- Define Circle type as a tuple of Point and Double
type Circle = (Point, Double)

-- Rectangle vs. Rectangle Intersection
rectsIntersect :: BoundingBox -> BoundingBox -> Bool
rectsIntersect (left1, bottom1, right1, top1) (left2, bottom2, right2, top2) =
  if not (((snd $ snd bottom1) > (snd $ snd top2)) ||
    ((snd $ snd top1) < (snd $ snd bottom2)) ||
    ((fst $ fst left1) > (fst $ fst right2)) ||
    ((fst $ fst right1) < (fst $ fst left2)))
    then True
    else False

-- Check denominator between LineSegments  is different from zero
checkFiniteLine :: LineSegment -> LineSegment -> Bool
checkFiniteLine((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let denom =(y2 - y1) * (x4 - x3) - (x2 - x1) * (y4 - y3)
      ua = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x4 - x3)) / denom
      ub = ((x4 - x3) * (y3 - y1)) - ((y4 - y3) * (x4 - x3)) / denom
      in if (
        (ua < 0) ||
        (ua > 1) ||
        (ub < 0) ||
        (ub > 1)
        )
        then False
        else True


-- Rectangle vs. Line Intersection
rectLineIntersect :: LineSegment -> BoundingBox -> Bool
rectLineIntersect ((x1, y1), (x2, y2)) (left, bottom, right, top) =
  if (
    checkFiniteLine ((x1, y1), (x2, y2)) left == True ||
    checkFiniteLine ((x1, y1), (x2, y2)) bottom == True ||
    checkFiniteLine ((x1, y1), (x2, y2)) right == True ||
    checkFiniteLine ((x1, y1), (x2, y2)) top == True
    )
    then True
    else False

-- Compute the distance between two points
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x2 -x1)^2 + (y2 - y1)^2)

-- Circle vs. Cirlce Intersection
circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect ((x1, y1), r1) ((x2, y2), r2) =
  let dCircles = distance (x1, y1) (x2, y2)
      sumRadii = r1 + r2
      in if ((dCircles == sumRadii) ||
            (dCircles) < sumRadii)
            then True
            else False


-- Circle vs Line Interscetion
circleLineIntersect :: LineSegment -> Circle -> Bool
circleLineIntersect ((x1, y1), (x2, y2)) ((xc, yc), r) =
  let localp1 = (x1 - xc, y1 - yc)
      localp2 = (x2 - xc, y2 - yc)
      -- Calculate difference in local values
      p2minusp1 = (fst localp2 - fst localp1, snd localp2 - snd localp1)
      a = (fst p2minusp1)^2 + (snd p2minusp1)^2
      b = 2 * ((fst p2minusp1 * fst localp1) + (snd p2minusp1 * snd localp1))
      c = (fst localp1)^2 + (snd localp1)^2 - r^2
      delta = b^2 - (4 * a * c)
      in if (delta == 0) ||
        (delta > 0)
        then True
        else False
