module direction


data Direction = North | East | South | West

turnCW: Direction -> Direction
turnCW North = East
turnCW East = South
turnCW South = West
turnCW West = North
