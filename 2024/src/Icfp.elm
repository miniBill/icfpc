module Icfp exposing (Binary(..), Icfp(..), Unary(..), decodeInt, decodeString, edit, encodeInt, encodeString, parse, toString, view)

import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, alignTop, column, el, fill, paragraph, rgb, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html
import Html.Attributes
import Int64 exposing (Int64)
import Markdown.Parser
import Markdown.Renderer
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Theme
import UInt64 exposing (UInt64)


type Icfp
    = Bool Bool
    | Int Int64
    | String String
    | Unary Unary Icfp
    | Binary Binary Icfp Icfp
    | Ternary Icfp Icfp Icfp
    | Variable UInt64
    | Lambda UInt64 Icfp


type Unary
    = Negation
    | Not
    | StringToInt
    | IntToString


type Binary
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Modulo
    | LessThan
    | GreaterThan
    | Equals
    | Or
    | And
    | Concat
    | Take
    | Drop
    | CallByName
    | CallLazy
    | CallStrict


parse : String -> Result (List Parser.DeadEnd) Icfp
parse input =
    Parser.run parser input


parser : Parser Icfp
parser =
    Parser.oneOf
        [ booleanParser
        , stringParser
        , integerParser
        , lambdaParser
        , variableParser
        , unaryParser
        , binaryParser
        , ternaryParser
        ]


variableParser : Parser Icfp
variableParser =
    Parser.succeed Variable
        |. Parser.symbol "v"
        |= innerIntegerParser


lambdaParser : Parser Icfp
lambdaParser =
    Parser.succeed Lambda
        |. Parser.symbol "L"
        |= innerIntegerParser
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)


unaryParser : Parser Icfp
unaryParser =
    Parser.succeed Unary
        |. Parser.symbol "U"
        |= unaryOperationParser
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)


unaryOperationParser : Parser Unary
unaryOperationParser =
    Parser.oneOf
        [ Parser.succeed Negation |. Parser.symbol "-"
        , Parser.succeed Not |. Parser.symbol "!"
        , Parser.succeed StringToInt |. Parser.symbol "#"
        , Parser.succeed IntToString |. Parser.symbol "$"
        ]


binaryParser : Parser Icfp
binaryParser =
    Parser.succeed Binary
        |. Parser.symbol "B"
        |= binaryOperationParser
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)


binaryOperationParser : Parser Binary
binaryOperationParser =
    Parser.oneOf
        [ Parser.succeed Addition |. Parser.symbol "+"
        , Parser.succeed Subtraction |. Parser.symbol "-"
        , Parser.succeed Multiplication |. Parser.symbol "*"
        , Parser.succeed Division |. Parser.symbol "/"
        , Parser.succeed Modulo |. Parser.symbol "%"
        , Parser.succeed LessThan |. Parser.symbol "<"
        , Parser.succeed GreaterThan |. Parser.symbol ">"
        , Parser.succeed Equals |. Parser.symbol "="
        , Parser.succeed Or |. Parser.symbol "|"
        , Parser.succeed And |. Parser.symbol "&"
        , Parser.succeed Concat |. Parser.symbol "."
        , Parser.succeed Take |. Parser.symbol "T"
        , Parser.succeed Drop |. Parser.symbol "D"
        , Parser.succeed CallByName |. Parser.symbol "$"
        , Parser.succeed CallLazy |. Parser.symbol "~"
        , Parser.succeed CallStrict |. Parser.symbol "!"
        ]


ternaryParser : Parser Icfp
ternaryParser =
    Parser.succeed Ternary
        |. Parser.symbol "?"
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)


booleanParser : Parser Icfp
booleanParser =
    Parser.succeed Bool
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.symbol "T"
            , Parser.succeed False
                |. Parser.symbol "F"
            ]


integerParser : Parser Icfp
integerParser =
    Parser.succeed (\v -> Int ( True, v ))
        |. Parser.symbol "I"
        |= innerIntegerParser


innerIntegerParser : Parser UInt64
innerIntegerParser =
    Parser.Workaround.chompUntilEndOrBefore " "
        |> Parser.getChompedString
        |> Parser.map decodeInt


stringParser : Parser Icfp
stringParser =
    Parser.succeed String
        |. Parser.symbol "S"
        |= (Parser.chompWhile (\c -> c /= ' ')
                |> Parser.getChompedString
                |> Parser.map decodeString
           )


toString : Icfp -> String
toString icfp =
    innerToString icfp
        |> String.join " "


innerToString : Icfp -> List String
innerToString icfp =
    case icfp of
        String s ->
            [ "S" ++ encodeString s ]

        Bool True ->
            [ "T" ]

        Bool False ->
            [ "F" ]

        Int ( s, int ) ->
            if s then
                [ "I" ++ encodeInt int ]

            else
                [ "U" ++ unaryToString Negation
                , "I" ++ encodeInt int
                ]

        Unary op c ->
            ("U" ++ unaryToString op) :: innerToString c

        Binary op l r ->
            ("B" ++ binaryToString op) :: innerToString l ++ innerToString r

        Ternary c t f ->
            ("?" ++ toString c) :: innerToString t ++ innerToString f

        Variable v ->
            [ "v" ++ encodeInt v ]

        Lambda v b ->
            ("L" ++ encodeInt v) :: innerToString b


view : Int -> Icfp -> Element msg
view layer icfp =
    case icfp of
        String s ->
            case
                s
                    |> Markdown.Parser.parse
                    |> Result.mapError Debug.toString
                    |> Result.andThen
                        (\parsed ->
                            parsed
                                |> Markdown.Renderer.render
                                    Markdown.Renderer.defaultHtmlRenderer
                        )
            of
                Ok rendered ->
                    el
                        [ Border.width 1
                        , Theme.padding
                        , width fill
                        , alignTop
                        , layerBackground layer
                        ]
                        (Element.html
                            (Html.div
                                [ Html.Attributes.style "white-space" "pre-wrap"
                                , Html.Attributes.style "font-family" "monospace"
                                ]
                                rendered
                            )
                        )

                Err _ ->
                    s
                        |> String.split "\n"
                        |> List.map (\line -> paragraph [] [ text line ])
                        |> column
                            [ Border.width 1
                            , Theme.padding
                            , width fill
                            , alignTop
                            , layerBackground layer
                            ]

        Bool True ->
            el [ alignTop ] <| text "True"

        Bool False ->
            el [ alignTop ] <| text "False"

        Int i ->
            el [ alignTop ] <| text <| Int64.toString i

        Unary op c ->
            boxxxy layer (unaryToString op) [ c ]

        Binary op l r ->
            boxxxy layer (binaryToString op) [ l, r ]

        Ternary c t f ->
            boxxxy layer "?" [ c, t, f ]

        Variable i ->
            el [ alignTop ] <| text <| "v" ++ UInt64.toString i

        Lambda v b ->
            boxxxy layer "Lambda" [ Variable v, b ]


unaryToString : Unary -> String
unaryToString unary =
    case unary of
        Negation ->
            "-"

        Not ->
            "!"

        StringToInt ->
            "#"

        IntToString ->
            "$"


binaryToString : Binary -> String
binaryToString binary =
    case binary of
        Addition ->
            "+"

        Subtraction ->
            "-"

        Multiplication ->
            "*"

        Division ->
            "/"

        Modulo ->
            "%"

        LessThan ->
            "<"

        GreaterThan ->
            ">"

        Equals ->
            "="

        Or ->
            "|"

        And ->
            "&"

        Concat ->
            "."

        Take ->
            "T"

        Drop ->
            "D"

        CallByName ->
            "$"

        CallLazy ->
            "~"

        CallStrict ->
            "!"


boxxxy : Int -> String -> List Icfp -> Element msg
boxxxy layer label children =
    Theme.column
        [ layerBackground layer ]
        [ text label
        , wrappedRow [ Theme.spacing ] (List.map (view (layer + 1)) children)
        ]


layerBackground : Int -> Attribute msg
layerBackground layer =
    let
        color : Color
        color =
            case modBy 4 layer of
                0 ->
                    rgb 1 1 1

                1 ->
                    rgb 1 0.9 0.9

                2 ->
                    rgb 0.9 1 0.9

                _ ->
                    rgb 0.9 0.9 1
    in
    Background.color color


edit : Icfp -> Element Icfp
edit icfp =
    let
        wrap : Element msg -> Element msg
        wrap child =
            el
                [ width fill
                , Theme.padding
                , Border.width 1
                ]
                child

        inner =
            case icfp of
                String s ->
                    Input.multiline
                        [ width fill
                        , alignTop
                        ]
                        { label = Input.labelHidden "String"
                        , text = s
                        , spellcheck = False
                        , placeholder = Nothing
                        , onChange = String
                        }
                        |> wrap

                Bool b ->
                    Input.radioRow
                        [ width fill
                        , alignTop
                        ]
                        { label = Input.labelHidden "Bool"
                        , onChange = Bool
                        , options =
                            [ Input.option True (text "True")
                            , Input.option False (text "False")
                            ]
                        , selected = Just b
                        }
                        |> wrap

                Int i ->
                    Input.text
                        [ width fill
                        , alignTop
                        ]
                        { label = Input.labelHidden "Int"
                        , text = Int64.toString i
                        , placeholder = Nothing
                        , onChange = \newInt -> Int (Maybe.withDefault i (Int64.fromString newInt))
                        }
                        |> wrap

                Unary op c ->
                    Theme.column []
                        [ text "Icfp.edit - branch 'Unary _ _' not fully implemented"
                        , Element.map (\nc -> Unary op nc) (edit c)
                        ]

                Binary op l r ->
                    Theme.column []
                        [ text "Icfp.edit - branch 'Binary _ _ _' not fully implemented"
                        , Element.map (\nl -> Binary op nl r) (edit l)
                        , Element.map (\nr -> Binary op l nr) (edit r)
                        ]

                Ternary c t f ->
                    Theme.column []
                        [ text "Icfp.edit - branch 'Ternary _ _ _' not fully implemented"
                        , Element.map (\nc -> Ternary nc t f) (edit c)
                        , Element.map (\nt -> Ternary c nt f) (edit t)
                        , Element.map (\nf -> Ternary c t nf) (edit f)
                        ]

                Variable v ->
                    Input.text
                        [ width fill
                        , alignTop
                        ]
                        { label = Input.labelHidden "Variable"
                        , text = UInt64.toString v
                        , placeholder = Nothing
                        , onChange = \newInt -> Variable (Maybe.withDefault v (UInt64.fromString newInt))
                        }
                        |> wrap

                Lambda v b ->
                    Theme.row []
                        [ Input.text [ alignTop ]
                            { text = UInt64.toString v
                            , placeholder = Nothing
                            , onChange = \newV -> Lambda (Maybe.withDefault v <| UInt64.fromString newV) b
                            , label = Input.labelHidden "Variable"
                            }
                        , edit b
                        ]
    in
    column [ width fill ]
        [ Input.radioRow
            [ width fill
            , spacing 16
            , Theme.padding
            , Border.widthEach
                { top = 1
                , left = 1
                , right = 1
                , bottom = 0
                }
            ]
            { label = Input.labelHidden "Kind"
            , onChange = identity
            , selected = Just icfp
            , options = toKindOptions icfp
            }
        , inner
        ]


toKindOptions : Icfp -> List (Input.Option Icfp Icfp)
toKindOptions icfp =
    let
        zero : Icfp
        zero =
            Int Int64.zero

        default =
            { bool = Bool True
            , int = zero
            , string = String ""
            , unary = Unary Negation icfp
            , binary = Binary Equals icfp zero
            , ternary = Ternary icfp zero zero
            , variable = Variable UInt64.zero
            , lambda = Lambda UInt64.zero icfp
            }

        options =
            case icfp of
                Bool _ ->
                    { default | bool = icfp }

                Int _ ->
                    { default | int = icfp }

                String _ ->
                    { default | string = icfp }

                Unary _ _ ->
                    { default | unary = icfp }

                Binary _ _ _ ->
                    { default | binary = icfp }

                Ternary _ _ _ ->
                    { default | ternary = icfp }

                Variable _ ->
                    { default | variable = icfp }

                Lambda _ _ ->
                    { default | lambda = icfp }
    in
    [ Input.option options.bool (text "Bool")
    , Input.option options.int (text "Int")
    , Input.option options.string (text "String")
    , Input.option options.unary (text "Unary")
    , Input.option options.binary (text "Binary")
    , Input.option options.ternary (text "Ternary")
    , Input.option options.variable (text "Variable")
    , Input.option options.lambda (text "Lambda")
    ]



{- INT -}


ninetyFour : UInt64
ninetyFour =
    UInt64.fromInt 94


decodeInt : String -> UInt64
decodeInt s =
    s
        |> String.toList
        |> List.foldl
            (\c acc ->
                UInt64.mul acc ninetyFour
                    |> UInt64.add (UInt64.fromInt <| Char.toCode c - 33)
            )
            UInt64.zero


encodeInt : UInt64 -> String
encodeInt i =
    let
        go : UInt64 -> List Char -> String
        go rest acc =
            if UInt64.isZero rest then
                String.fromList acc

            else
                let
                    ( div, mod ) =
                        UInt64.divMod rest ninetyFour
                in
                go
                    div
                    (Char.fromCode (33 + Maybe.withDefault 0 (UInt64.toInt31 mod)) :: acc)
    in
    go i []



{- STRING -}


stringCodecPairs : List ( Char, Char )
stringCodecPairs =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
        |> String.toList
        |> List.indexedMap (\i c -> ( Char.fromCode (i + 33), c ))


stringDecodingDict : Dict Char Char
stringDecodingDict =
    Dict.fromList stringCodecPairs


stringEncodingDict : Dict Char Char
stringEncodingDict =
    stringCodecPairs
        |> List.map (\( k, v ) -> ( v, k ))
        |> Dict.fromList


decodeString : String -> String
decodeString input =
    input
        |> String.toList
        |> List.filterMap (\c -> Dict.get c stringDecodingDict)
        |> String.fromList


encodeString : String -> String
encodeString input =
    input
        |> String.toList
        |> List.filterMap (\c -> Dict.get c stringEncodingDict)
        |> String.fromList
