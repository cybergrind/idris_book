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
