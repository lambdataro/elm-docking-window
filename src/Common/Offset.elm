module Common.Offset exposing (HasOffset, Offset, subOffset, subPos, zeroOffset)

import Common.Pos exposing (HasPos)


type alias HasOffset number a =
    { a
        | offsetX : number
        , offsetY : number
    }


type alias Offset number =
    HasOffset number {}


zeroOffset : Offset number
zeroOffset =
    { offsetX = 0
    , offsetY = 0
    }


{-| posからposを引いてoffsetとして返す
-}
subPos : HasPos number a -> HasPos number b -> Offset number
subPos pos1 pos2 =
    { offsetX = pos1.x - pos2.x
    , offsetY = pos1.y - pos2.y
    }


{-| posからoffsetを引いたposを返す
-}
subOffset : HasPos number a -> HasOffset number b -> HasPos number a
subOffset record { offsetX, offsetY } =
    { record
        | x = record.x - offsetX
        , y = record.y - offsetY
    }
