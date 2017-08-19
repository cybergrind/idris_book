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


||| transpose matrix
trans: Vect m (Vect n elem) -> Vect n (Vect m elem)
trans [] = createEmpties
trans (x :: xs) =
      let xsTrans = trans xs in
      zipWith (::) x xsTrans



dotprod : Num numType => (x : Vect m numType) -> (y : Vect m numType) -> numType
dotprod [] _ = 0
dotprod (x :: xs) (y :: ys) = x * y + dotprod xs ys


iter : Num numType =>
  (x : Vect m numType) -> (ys: Vect p (Vect m numType)) ->
  Vect p numType
iter x [] = []
iter x (y :: xs) = dotprod x y :: iter x xs



mulTransposed : Num numType =>
     (xs : Vect n (Vect m numType)) ->
     (ys : Vect p (Vect m numType)) -> Vect n (Vect p numType)
mulTransposed (x :: xs) ys = iter x ys :: mulTransposed xs ys
mulTransposed [] _ = []


-- mulM [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]
-- should give: [[29, 32, 35, 38], [65, 72, 79, 86], [101, 112, 123, 134]]

||| multiply matrixes
total mulM : Num numType =>
     Vect n (Vect m numType) -> Vect m (Vect p numType) ->
     Vect n (Vect p numType)
mulM [] [] = []
mulM xs ys = let yst = trans ys in
     mulTransposed xs yst
