module Common.Pos exposing (HasPos, Pos, zeroPos)


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
