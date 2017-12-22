{-
  File      :  Recursion.hs
  Copyright : (c) Juan Arroyo-Miranda, 10/02/17
  Contains types for defining points in 2D, 3D, and 4D. It also contains
  different functions to modify lists recursively
-}

data PointTy = Point2D | Point3D | Point4D
  deriving (Show, Eq)
data Point a = Point PointTy [a]
  deriving (Show, Eq)

{-
  Takes in a list and a value and returns a list where the value
  appears before and after every element in the list
-}
intersperse :: [a] -> a -> [a]
intersperse [] scalar = [scalar]
intersperse (x:xs) scalar = scalar : x : intersperse xs scalar


{-
Takes in two lists and weaves the elements of lists together.
If either list is exhausted before the other then add the remaining elements
of the non-exhausted list to the returned list.

-}
weave :: [a] -> [a] -> [a]
weave [] ys = ys
weave xs [] = xs
weave (x:xs) (y:ys) = x : y : weave xs ys

-- Swaps the two elements in a tuple
doOneSwap :: (a,b) -> (b, a)
doOneSwap (x,y) = (y,x)

{-

Takes a list of pairs and returns a list of pairs. The returned list matches
the input list except that the elements of each pair have had their
positions swapped.
-}

pairSwap :: [(a,b)] -> [(b, a)]
pairSwap [] = []
pairSwap [(x1, y1), (x2, y2)] = [doOneSwap  (x1, y1), doOneSwap (x2, y2)]

-- Reverse two lists
revereseTwo :: [a] -> [a] -> [a]
revereseTwo [] ys = ys
revereseTwo (x:xs) ys = revereseTwo xs (x:ys)


{-
Takes in a list and simply put, reverses the elements in the list.
-}

reverse' :: [a] -> [a]
reverse' xs = revereseTwo xs []

{-
Takes a list of pairs and returns a pair of lists. The returned pair of lists
 contains a list of the first elements of the input pair ( a ) and a list of
 the second elements of the input pairs ( b ).
-}

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a,b):xs) =
  let (as,bs) = unzip' xs
   in (a:as, b:bs)


{-
Takes two input lists of a and b and returns a list of ( a , b ) pairs.
-}

zip':: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' _ [] = error "list is not exhaustive"
zip' [] _ = error "list is not exhaustive"
zip' (x:xs) (y:ys) = (x,y):zip' xs ys



{-
Takes a predicate (a function) and a list as arguments.
This function must return a pair of lists.
-}
splitFilter :: (a -> Bool) -> [a] -> ([a], [a])
splitFilter _ []   = ([], [])
splitFilter func (x:z)
  | func x         =  (x:xs, ys)
  | otherwise      =  (xs, x:ys)
  where (xs, ys) = splitFilter func z


{-
Takes in a point type and a list of points and returns a list all points
that match the point type in the input list.
-}

findPoint :: PointTy -> [Point a] -> [Point a]
findPoint _ [] = []
findPoint Point2D ((Point Point2D coords):pts) = (Point Point2D coords):findPoint Point2D pts
findPoint Point3D ((Point Point3D coords):pts) = (Point Point3D coords):findPoint Point3D pts
findPoint Point4D ((Point Point4D coords):pts) = (Point Point4D coords):findPoint Point4D pts
findPoint ty ((_):pts) = findPoint ty pts


-- Helper function for computing length

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Helper function for computing True and False matches in patterns

findMeTrue :: [Point a] -> [Bool]
findMeTrue [] = []
findMeTrue ((Point Point2D coords):pts)
 | length' coords == 2 = True:findMeTrue pts
 | otherwise           = findMeTrue pts
findMeTrue ((Point Point3D coords):pts)
 | length' coords == 3 = True:findMeTrue pts
 | otherwise           = findMeTrue pts
findMeTrue ((Point Point4D coords):pts)
 | length' coords == 4 = True:findMeTrue pts
 | otherwise           = findMeTrue pts


{-
Takes in a list of points and returns true if every point in the list has
the correct number coordinates
-}

isValid :: [Point a] -> Bool
isValid x
  | length' x == length' truevalues   = True
  | otherwise                         = False
  where truevalues = findMeTrue x


-- Helper function for finding Points in 2D

processTri :: [Point Double] -> Bool
processTri ((Point Point2D coords1):(Point Point2D coords2):(Point Point2D coords3):[])
         | (l1 == 2) && (l2 == 2) && (l3 == 2) = True
         | otherwise           = False
         where l1 = length' coords1
               l2 = length' coords2
               l3 = length' coords3
processTri _ = False

-- Helper function to get coordinates from points in 2D

getIt :: [Point Double] -> [[Double]]
getIt [] = []
getIt ((Point Point2D coords):xs) = coords:getIt xs
getIt (_:xs) = error "Should only accept Point 2Ds"

-- Helper function for computing distance

distance :: [Double] -> [Double] -> Double
distance [x1, y1] [x2, y2] = sqrt((x2-x1)**2 + (y2-y1)**2)

-- Helper function to extract the maximum from a list

maximum' :: Ord a => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

-- Helper function to remove element from a list

removeItem :: Double -> [Double] -> [Double]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Helper function to compute the sum of squared elements from a list

sumSq :: [Double] -> Double
sumSq [] = 0
sumSq (x:xs) = x^2 + sumSq xs

-- Helper function to check whether list conforms to Pythagorean Theorem

littlePythagoras :: [[Double]] -> Bool
littlePythagoras (first:second:third:[])
    |hypo^2 == absq = True
    |otherwise      = False
    where num1 = distance first second
          num2 = distance first third
          num3 = distance second third
          hypo = maximum' [num1, num2, num3]
          extra = removeItem hypo [num1, num2, num3]
          absq = sumSq extra


{- Helper function to encapsulte 2D Points and whether they conform
to Pythagorean Theorem
-}

wrapper :: [Point Double] -> Bool
wrapper x
 | (length' x ==3) && (processTri x == True) = littlePythagoras (getIt x)
 | otherwise                                 = False

{-
Helper for getting a list of Booleans for the lists of Points
True if list is made out of 2D points and Pythagorean
False otherwise
-}

findRightBool :: [[Point Double]] -> [Bool]
findRightBool [] = []
findRightBool (x:xs)
  | wrapper x == True    = True:findRightBool xs
  | otherwise            = False:findRightBool xs

-- Get indices for True Point lists

getMeIndices :: [(Integer, Bool)] -> [Integer]
getMeIndices [] = []
getMeIndices ((c, True):xs) = c:getMeIndices xs
getMeIndices ((c,_):xs) = getMeIndices xs


-- Wrapps all functions to return the index list

masterWrapper :: [[Point Double]] -> [Integer]
masterWrapper [] = []
masterWrapper x =
  let bools = findRightBool x
      upper = length' bools -1
  in  (getMeIndices $ zip' [0..upper] bools)

{-
Takes in a list of point lists and returns a Maybe list of the indices that
represent the index of each point list that creates a right triangle.
-}

findRightTris :: [[Point Double]] -> Maybe [Integer]
findRightTris [] = Nothing
findRightTris x
 | (length' x > 0) && (result /=[])  = Just result
 | otherwise                         = Nothing
 where result = masterWrapper x
