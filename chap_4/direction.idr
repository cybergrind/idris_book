module direction


data Direction = North | East | South | West

turnCW: Direction -> Direction
turnCW North = East
turnCW East = South
turnCW South = West
turnCW West = North


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double


area: Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x


data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture


rectangle : Picture
rectangle = Primitive $ Rectangle 10 10

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 15 20)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))


%name Picture pic1, pic2, pic3
%name Shape shape1, shape2, shape3

pictureArea: Picture -> Double
pictureArea (Primitive shape1) = area shape1
pictureArea (Combine pic1 pic2) = (pictureArea pic1) + (pictureArea pic2)
pictureArea (Rotate x pic1) = pictureArea pic1
pictureArea (Translate x y pic1) = pictureArea pic1


biggestTriangle: Picture -> Maybe Double
biggestTriangle (Primitive t@(Rectangle x y)) = Just (area t)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = liftA2 max (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate x pic1) = biggestTriangle pic1
biggestTriangle (Translate x y pic1) = biggestTriangle pic1
