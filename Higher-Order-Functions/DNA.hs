{-
  File      :  DNA.hs
  Copyright : (c) Juan Arroyo, 10/09/17
  Contains Eq and Show instances for DNA definitions.
  Also contains functions for producing a Helix and checking whether
  two Strands align.
-}

{-
   --- Definitions for Base, BasePair, Strand and Helix
-}

data Base = A | C | G | T
data BasePair = BasePair Base Base
data Strand = Strand [Base]
data Helix = Helix [BasePair]




{-
    Define Show and Eq instances for Base, BasePair, Strand, and Helix.

    The show for a "Base" value should be the strings: "A" or "T" or "C" or "G"
    The show for a "BasePair" value should be formatted using tuple syntax with the base letter. For example: "(A,T)" or "(G,C)"
    The show for a "Strand" value should be formatted using list syntax with the base letters. For example: "[A,B,C,T]"
    The show for a "Helix" value should be formatted using tuple syntax and list syntax with the base letters. For example: "[(A,B),(C,T)]"

-}

instance Eq Base where
  (==) A A = True
  (==) C C = True
  (==) G G = True
  (==) T T = True
  (==) _ _ = False

instance Show Base where
  show A = "A"
  show C = "C"
  show G = "G"
  show T = "T"

instance Eq BasePair where
  (==) (BasePair x0 y0) (BasePair x1 y1) = x0 == x1 && y0 == y1

instance Show BasePair where
  show (BasePair base1 base2) = "(" ++ show base1 ++ ", " ++ show base2 ++ ")"

instance Eq Strand where
  (==) (Strand l1) (Strand l2) = l1 == l2

instance Show Strand where
  show (Strand base)  = show base

instance Eq Helix where
  (==) (Helix h1) (Helix h2) = h1 == h2

instance Show Helix where
  show (Helix basepairl) = show basepairl

{-
    wccHelix :: Strand -> Helix.
    Given a Strand, generate a Helix.

    Requirements
    -------------
    1. This function must use list comprehension somewhere. You can choose.
    2. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.
    3. You can define helper functions but they can only use HOFs if they return lists.

    Hint: This function can be written in one line.

    Example:
    Main*> wccHelix (Strand [A,T,C,G])
    [(A,T),(T,A),(C,G),(G,C)]
-}


-- Find Base complement
findComplement :: Base -> Base
findComplement A = T
findComplement T = A
findComplement C = G
findComplement G = C

wccHelix :: Strand -> Helix
wccHelix (Strand lst) = Helix [BasePair x (findComplement x) | x <- lst]




{-
    makeHelix :: String -> Helix.
    Given a String of base letters, make a Helix.

    Requirements
    -------------
    1. This function must use list comprehension somewhere. You can choose.
    2. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.
    3. You can define helper functions but they can only use HOFs if they return lists.
    4. You can reuse functions defined in this file.


    Hint: This function can be written in one line.


    Example:
    Main*> makeHelix â€œACTG"
    [(A,T),(C,G),(T,A),(G,C)]
-}

-- Find complement for a character type
toString :: Char -> Base
toString 'A' = A
toString 'T' = T
toString 'C' = C
toString 'G' = G

makeHelix :: String -> Helix
makeHelix str =  Helix (let strlst = [toString x | x<-str ] in [BasePair x (findComplement x) | x <- strlst])


{-
   willAnneal :: Strand -> Strand -> Bool.
   Determine whether two strands will perfectly anneal (i.e., every base is a Watson-Crick complementarity).

    Requirements
    -------------
    1. The function can only use HOFs. You cannot use Prelude list functions. You can redefine Prelude (Data.List) functions but they must be redefined using HOFs.
    2. You can define helper functions but they can only use HOFs, if they return lists.
    3. You can reuse functions defined in this file.


   Main*> willAnneal (Strand [A,C,T,G]) (Strand [T,G,A,C])
   True
   Main*> willAnneal (Strand [A,C,T,T]) (Strand [T,G,A,C])
   False
-}

willAnneal :: Strand -> Strand -> Bool
willAnneal (Strand one) (Strand two)
  | truecomp == (Strand two) = True
  | otherwise                = False
  where truecomp = (Strand (map findComplement one))
