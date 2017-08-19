
import Data.Vect

fourInts: Vect 4 Int
fourInts = [1, 2, 3, 4]

sixInts: Vect 6 Int
sixInts = [5, 6, 7, 8, 9, 10]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts


total allLengths: Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words



insSort : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insSort x [] = [x]
-- insSort x (y :: xs) = 
--         if x < y 
--          then x :: y :: xs
--          else y :: insSort x xs
insSort x (y :: xs) = case x < y of
                           False => y :: insSort x xs
                           True => x :: y :: xs

total sortVec: Ord elem => Vect n elem -> Vect n elem
sortVec [] = []
sortVec (x :: xs) = 
        let xsSorted = sortVec xs in
        insSort x xsSorted
                  


mylen: List a -> Nat
mylen [] = 0
mylen (x :: xs) = 1 + mylen xs


myrev: List a -> List a
myrev [] = []
myrev (x :: xs) = myrev xs ++ [x]

total mymap : (a -> b) -> List a -> List b
mymap f [] = []
mymap f (x :: xs) = f x :: mymap f xs

mymapv : (a -> b) -> Vect n a -> Vect n b
mymapv f [] = []
mymapv f (x :: xs) = f x :: mymapv f xs

