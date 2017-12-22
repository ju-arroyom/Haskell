{-
  File      :  HigherOrder.hs
  Copyright : (c) Juan Arroyo, 10/09/17

-}

{-
Takes a Int list and a range that is represent by a Int tuple and returns a Int
list of the elements inside the input list within a given range (inclusive).
The elements of the returned list must be in the same order they appeared
in the input list.
-}


listRange :: [Int] -> (Int,Int) -> [Int]
listRange xs (a, b) =
  let interlst = [filter (\x -> x == y) [a..b] | y <- xs]
  in [w |z <- interlst, w <- z]



{-
Takes in a list of Int lists and returns the sum of all the numbers
inside the list of Int lists.
-}
maxSum :: [[Int]] -> Int
maxSum inputlst = foldl (+) 0 [foldl (+) 0 x | x<-inputlst]


-- Helper function to take head of list
head' :: [a] -> a
head' [] = error "Empty list does not have head !!!"
head' (x:xs) = x

-- Helper function to take tail of list
tail' :: [Int] -> [Int]
tail' [] = error "Empty list does not have tail !!!"
tail' (x:xs) = xs


{-
Takes in two arguments: a function that represents a equivalence relation,
and a Int list. The function returns a list containing the same elements as
the input list, but without any duplicates
-}

dedupe :: (Int -> Int -> Bool) -> [Int] -> [Int]
dedupe f lst =
  snd $ foldr (\ x (prior, accum) -> (x, (if (f prior x) then accum else x:accum ))) (head' lst, []) (tail' lst)


{-
Takes in a Int list and returns a list of all non-empty prefixes of a
list, ordered from shortest to longest.

Note : Consulted https://stackoverflow.com/questions/8469467/haskell-help-understanding-a-function
to better understand map (x:)
-}

prefixes :: [Int] -> [[Int]]
prefixes lst =
  let input = foldr ( \ x y -> [] : (map (x:) y)) [[]] lst
  in filter (\x -> x /= [] ) input



-- Recursive implementation of take function
take' :: [Int] -> Int -> [Int]
take' _ n            | n <= 0 =  []
take' [] n           =  []
take' (x:xs) n       =  x : take' xs  (n-1)

-- Helper function to take all sublists of size n from a list

sublists :: [Int] -> Int -> [[Int]]
sublists [] _ = [[]]
sublists xs@(_:rest) n = take' xs n : sublists rest n

--Helper function to compute length

length' :: [Int] -> Int
length' xs = foldl (\acc _ -> 1 + acc) 0 xs

{-
Takes in a Int list and a Int . The function returns the contiguous sublist of
length k whose elements have the largest sum.
-}

kSublist :: [Int] -> Int -> [Int]
kSublist xs n =
  let subxs    = filter (\ x -> length' x == n) $ sublists xs n
      ordered  = foldl (\(param1, accum) x -> (foldl (+) 0  x, (if (foldl (+) 0 x) > param1 then x:accum else accum))) (0,[]) subxs
      in head' $ snd $ ordered
