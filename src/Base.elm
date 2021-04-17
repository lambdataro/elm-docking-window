module Base exposing (Flags, Model, Msg(..), init, subscriptions, update)

import Browser.Events
import Common.Pos exposing (Pos, zeroPos)
import Common.Size exposing (Size)
import Json.Decode as Dec exposing (Decoder)



-- MODEL


type alias Flags =
    { winWidth : Float
    , winHeight : Float
    }


type alias Model =
    { winSize : Size Float
    , mousePos : Pos Float
    }


init : Flags -> Model
init { winWidth, winHeight } =
    { winSize = { width = winWidth, height = winHeight }
    , mousePos = zeroPos
    }



-- SUBSCRIPTION


resizeMsg : (Size Float -> a) -> Int -> Int -> a
resizeMsg f width height =
    f
        { width = toFloat width
        , height = toFloat height
        }


mousePosDecoder : (Pos Float -> a) -> Decoder a
mousePosDecoder f =
    Dec.map2 (\x y -> f { x = x, y = y })
        (Dec.field "offsetX" Dec.float)
        (Dec.field "offsetY" Dec.float)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (resizeMsg BrowserResize)
        , Browser.Events.onMouseDown (mousePosDecoder MouseDown)
        , Browser.Events.onMouseMove (mousePosDecoder MouseMove)
        , Browser.Events.onMouseUp (mousePosDecoder MouseUp)
        ]



-- UPDATE


type Msg
    = BrowserResize (Size Float)
    | MouseDown (Pos Float)
    | MouseMove (Pos Float)
    | MouseUp (Pos Float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        BrowserResize winSize ->
            { model | winSize = winSize }

        MouseDown _ ->
            model

        MouseMove mousePos ->
            { model | mousePos = mousePos }

        MouseUp _ ->
            model
