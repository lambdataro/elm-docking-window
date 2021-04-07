module Common.Size exposing (HasSize, Size, zeroSize)


type alias HasSize number a =
    { a
        | width : number
        , height : number
    }


type alias Size number =
    HasSize number {}


zeroSize : Size number
zeroSize =
    { width = 0
    , height = 0
    }
