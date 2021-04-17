module Parts.DraggableRect exposing (DraggableRect, draggableRectSvg)

import Common.Pos exposing (HasPos)
import Common.Size exposing (Size)
import Parts.Style exposing (rectFill)
import Parts.SvgUtils exposing (mouseDownEvent, posAttr, sizeAttr)
import TypedSvg as Svg
import TypedSvg.Attributes as Attr
import TypedSvg.Core exposing (Svg)


type alias DraggableRect =
    HasPos Float (Size Float)


draggableRectSvg : DraggableRect -> msg -> Svg msg
draggableRectSvg rect mouseDownMsg =
    Svg.rect
        (posAttr rect <| sizeAttr rect [ Attr.fill rectFill, mouseDownEvent mouseDownMsg ])
        []
