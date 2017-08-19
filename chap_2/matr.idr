module matr
import Data.Vect


addMCol : Num numType => Vect cols numType -> Vect cols numType -> Vect cols numType
addMCol [] [] = []
addMCol (x :: xs) (y :: ys) = x + y :: addMCol xs ys

total addM : Num numType =>
     Vect rows (Vect cols numType) ->
     Vect rows (Vect cols numType) ->
     Vect rows (Vect cols numType)
addM [] [] = []
addM (x :: xs) (y :: ys) = addMCol x y :: addM xs ys


createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []


trans: Vect m (Vect n elem) -> Vect n (Vect m elem)
trans [] = createEmpties
trans (x :: xs) = 
      let xsTrans = trans xs in
      zipWith (::) x xsTrans


mulTransposed : Num numType => 
     (xs : Vect n (Vect m numType)) ->  
     (ys : Vect p (Vect m numType)) -> Vect n (Vect p numType)
mulTransposed (x :: xs) ys = ?hole
mulTransposed [] _ = []



mulM : Num numType =>
     Vect n (Vect m numType) -> Vect m (Vect p numType) ->
     Vect n (Vect p numType)
mulM [] [] = []
mulM xs ys = let yst = trans ys in
     mulTransposed xs yst


