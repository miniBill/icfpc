module Theme exposing (button, column, padding, row, spacing)

import Element exposing (Attribute, Element, alignTop, fill, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


rhythm : number
rhythm =
    8


padding : Attribute msg
padding =
    Element.padding rhythm


spacing : Attribute msg
spacing =
    Element.spacing rhythm


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Element.row (Border.width 1 :: alignTop :: width fill :: padding :: spacing :: attrs) children


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Element.column (Border.width 1 :: alignTop :: width fill :: padding :: spacing :: attrs) children


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs config =
    Input.button (Border.width 1 :: padding :: width fill :: Font.center :: attrs) config
