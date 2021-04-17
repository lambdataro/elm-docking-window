module Parts.SvgUtils exposing (mouseDownEvent, posAttr, sizeAttr, svgViewBox)

import Common.Pos exposing (HasPos)
import Common.Size exposing (HasSize)
import Html exposing (Html)
import Json.Decode as Dec
import TypedSvg as Svg
import TypedSvg.Attributes as Attr
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types as SvgType
import VirtualDom


svgViewBox : HasSize Float a -> List (Svg msg) -> Html msg
svgViewBox { width, height } =
    Svg.svg [ Attr.viewBox 0 0 width height ]


{-| xとyのattributeをリストに追加する
-}
posAttr : HasPos Float a -> List (Attribute msg) -> List (Attribute msg)
posAttr { x, y } attrList =
    Attr.x (SvgType.Num x)
        :: Attr.y (SvgType.Num y)
        :: attrList


{-| widthとheightのattributeをリストに追加する
-}
sizeAttr : HasSize Float a -> List (Attribute msg) -> List (Attribute msg)
sizeAttr { width, height } attrList =
    Attr.width (SvgType.Num width)
        :: Attr.height (SvgType.Num height)
        :: attrList


{-| SVGの要素に渡すmouseDownイベント
-}
mouseDownEvent : msg -> Attribute msg
mouseDownEvent msg =
    SvgEvents.on "mousedown" <|
        VirtualDom.Custom <|
            Dec.succeed
                { message = msg
                , stopPropagation = True -- windowのmousedownが発火しないようにする
                , preventDefault = False
                }
