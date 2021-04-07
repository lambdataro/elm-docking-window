module Main exposing (main)

import Base exposing (Flags)
import Browser exposing (Document)
import Common.Utils exposing (withCmdNone)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { base : Base.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    withCmdNone { base = Base.init flags }



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map BaseMsg <| Base.subscriptions model.base



-- UPDATE


type Msg
    = BaseMsg Base.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BaseMsg baseMsg ->
            withCmdNone { model | base = Base.update baseMsg model.base }



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Docking Window System"
    , body = []
    }
