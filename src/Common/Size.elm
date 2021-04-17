module Common.Size exposing (HasSize, Size, updateSize, zeroSize)


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


updateSize : HasSize number a -> HasSize number b -> HasSize number b
updateSize { width, height } record =
    { record
        | width = width
        , height = height
    }
