module Parts.Background exposing (backgroundSvg)

import Base
import Parts.Style exposing (backgroundFill)
import Parts.SvgUtils exposing (sizeAttr)
import TypedSvg as Svg
import TypedSvg.Attributes as Attr
import TypedSvg.Core exposing (Svg)


{-| ウインドウサイズに追従する背景(Rect)
-}
backgroundSvg : Base.Model -> Svg msg
backgroundSvg baseModel =
    Svg.rect
        (sizeAttr baseModel.winSize [ Attr.fill backgroundFill ])
        []
