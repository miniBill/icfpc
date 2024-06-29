module Icfp exposing (Binary(..), Icfp(..), Unary(..), decodeInt, decodeString, edit, encodeInt, encodeString, parse, toString, view)

import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, alignTop, column, el, fill, paragraph, rgb, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html
import Html.Attributes
import Markdown.Parser
import Markdown.Renderer
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Theme


type Icfp
    = Bool Bool
    | Int Int
    | String String
    | Unary Unary Icfp
    | Binary Binary Icfp Icfp
    | Ternary Icfp Icfp Icfp
    | Variable Int
    | Lambda Int Icfp


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
    | Apply


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
        , Parser.succeed Apply |. Parser.symbol "$"
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
    Parser.succeed Int
        |. Parser.symbol "I"
        |= innerIntegerParser


innerIntegerParser : Parser Int
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

        Int int ->
            if int < 0 then
                innerToString (Unary Negation (Int -int))

            else
                [ "I" ++ encodeInt int ]

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
            el [ alignTop ] <| text <| String.fromInt i

        Unary op c ->
            boxxxy layer (unaryToString op) [ c ]

        Binary op l r ->
            boxxxy layer (binaryToString op) [ l, r ]

        Ternary c t f ->
            boxxxy layer "?" [ c, t, f ]

        Variable 0 ->
            el [ alignTop ] <| text "x"

        Variable 1 ->
            el [ alignTop ] <| text "y"

        Variable 2 ->
            el [ alignTop ] <| text "z"

        Variable 3 ->
            el [ alignTop ] <| text "w"

        Variable i ->
            el [ alignTop ] <| text <| "v" ++ String.fromInt i

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

        Apply ->
            "$"


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
                        , text = String.fromInt i
                        , placeholder = Nothing
                        , onChange = \newInt -> Int (Maybe.withDefault i (String.toInt newInt))
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
                        , text = String.fromInt v
                        , placeholder = Nothing
                        , onChange = \newInt -> Variable (Maybe.withDefault v (String.toInt newInt))
                        }
                        |> wrap

                Lambda v b ->
                    Theme.row []
                        [ Input.text [ alignTop ]
                            { text = String.fromInt v
                            , placeholder = Nothing
                            , onChange = \newV -> Lambda (Maybe.withDefault v <| String.toInt newV) b
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
        default =
            { bool = Bool True
            , int = Int 0
            , string = String ""
            , unary = Unary Negation icfp
            , binary = Binary Equals icfp (Int 0)
            , ternary = Ternary icfp (Int 0) (Int 0)
            , variable = Variable 0
            , lambda = Lambda 0 icfp
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


decodeInt : String -> Int
decodeInt s =
    s
        |> String.toList
        |> List.foldl (\c acc -> acc * 94 + (Char.toCode c - 33)) 0


encodeInt : Int -> String
encodeInt i =
    let
        go : Int -> List Char -> String
        go rest acc =
            if rest == 0 then
                String.fromList acc

            else
                go (rest // 94) (Char.fromCode (33 + modBy 94 rest) :: acc)
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
