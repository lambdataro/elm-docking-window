module Main exposing (main)

import Base exposing (Flags)
import Browser exposing (Document)
import Common.Offset exposing (Offset, subOffset, subPos)
import Common.Pos exposing (Pos, updatePos)
import Common.Utils exposing (withCmdNone)
import Parts.Background exposing (backgroundSvg)
import Parts.DraggableRect exposing (DraggableRect, draggableRectSvg)
import Parts.SvgUtils exposing (svgViewBox)



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
    , rect : DraggableRect
    , draggingStatus : DraggingStatus
    }


type DraggingStatus
    = NotDragging
    | Dragging (Offset Float)


init : Flags -> ( Model, Cmd Msg )
init flags =
    withCmdNone
        { base = Base.init flags
        , rect = { x = 50, y = 50, width = 100, height = 100 }
        , draggingStatus = NotDragging
        }



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map BaseMsg <| Base.subscriptions model.base



-- UPDATE


type Msg
    = BaseMsg Base.Msg
    | StartDragging


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BaseMsg baseMsg ->
            withCmdNone <|
                updateByBaseMessage baseMsg
                    { model | base = Base.update baseMsg model.base }

        StartDragging ->
            withCmdNone
                { model
                    | draggingStatus =
                        Dragging <|
                            -- rectのローカル座標でのマウスの位置を求める
                            subPos model.base.mousePos model.rect
                }


{-| Base.MsgによってModelをupdateする処理
-}
updateByBaseMessage : Base.Msg -> Model -> Model
updateByBaseMessage baseMsg model =
    case baseMsg of
        Base.MouseMove mousePos ->
            updateByMouseMove mousePos model

        Base.MouseUp mousePos ->
            updateByMouseUp mousePos model

        _ ->
            model


{-| マウスの移動でModelを更新する処理
-}
updateByMouseMove : Pos Float -> Model -> Model
updateByMouseMove mousePos model =
    case model.draggingStatus of
        NotDragging ->
            model

        Dragging offset ->
            { model | rect = updatePos (subOffset mousePos offset) model.rect }


{-| マウスアップでModelを更新する処理
-}
updateByMouseUp : Pos Float -> Model -> Model
updateByMouseUp _ model =
    { model | draggingStatus = NotDragging }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Docking Window System"
    , body =
        [ svgViewBox model.base.winSize
            [ backgroundSvg model.base
            , draggableRectSvg model.rect StartDragging
            ]
        ]
    }
