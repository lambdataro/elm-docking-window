module Common.Pos exposing (HasPos, Pos, updatePos, zeroPos)


type alias HasPos number a =
    { a
        | x : number
        , y : number
    }


type alias Pos number =
    HasPos number {}


zeroPos : Pos number
zeroPos =
    { x = 0
    , y = 0
    }


updatePos : HasPos number a -> HasPos number b -> HasPos number b
updatePos { x, y } record =
    { record
        | x = x
        , y = y
    }
