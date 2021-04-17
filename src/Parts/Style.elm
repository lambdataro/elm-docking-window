module Parts.Style exposing (backgroundFill, rectFill)

import Color exposing (Color)
import TypedSvg.Types as SvgType


hslColor : Float -> Float -> Float -> Color
hslColor hue saturation lightness =
    Color.hsl (hue / 360) (saturation / 100) (lightness / 100)


backgroundColor : Color
backgroundColor =
    hslColor 0 0 80


backgroundFill : SvgType.Paint
backgroundFill =
    SvgType.Paint backgroundColor


rectColor : Color
rectColor =
    Color.red


rectFill : SvgType.Paint
rectFill =
    SvgType.Paint rectColor
