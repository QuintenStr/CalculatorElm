module Main exposing (calculateResult, main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Parser exposing (..)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    let
        toMsg key =
            case key of
                "Backspace" ->
                    Backspace

                "Enter" ->
                    Calculate

                "c" ->
                    Clear

                "C" ->
                    Clear

                _ ->
                    if
                        List.member key
                            [ "0"
                            , "1"
                            , "2"
                            , "3"
                            , "4"
                            , "5"
                            , "6"
                            , "7"
                            , "8"
                            , "9"
                            , "A" -- Only uppercase hex digits, so no overlap for the clear input = c
                            , "B"
                            , "C"
                            , "D"
                            , "E"
                            , "F"
                            , "x"
                            , "o"
                            , "b"
                            , "+"
                            , "-"
                            , "*"
                            , "/"
                            , "("
                            , ")"
                            , "["
                            , "]"
                            , "{"
                            , "}"
                            , "."
                            ]
                    then
                        PressButton key

                    else
                        NoOp
    in
    Decode.map toMsg (Decode.field "key" Decode.string)



-- MODEL


type alias Model =
    { input : String
    , result : Result String Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , result = Ok 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = PressButton String
    | Calculate
    | Clear
    | Backspace
    | NoOp


calculateResult : String -> Result String Float
calculateResult input =
    if input == "" then
        Ok 0

    else
        case run expressionParser input of
            Ok expr ->
                let
                    result =
                        evaluateExpr expr
                in
                if isInfinite result || isNaN result then
                    Err "Division by zero or invalid operation"

                else
                    -- round to 10 decimal places to avoid floating point precision issues
                    Ok (toFloat (round (result * 1.0e10)) / 1.0e10)

            Err _ ->
                Err "Invalid expression"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressButton symbol ->
            ( { model | input = model.input ++ symbol }
            , Cmd.none
            )

        Calculate ->
            ( { model | result = calculateResult model.input }
            , Cmd.none
            )

        Clear ->
            ( { model
                | input = ""
                , result = Ok 0
              }
            , Cmd.none
            )

        Backspace ->
            ( { model
                | input =
                    if String.length model.input > 0 then
                        String.slice 0 (String.length model.input - 1) model.input

                    else
                        ""
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- PARSE AND EVALUATE


type Expr
    = Number Float
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    | Group Expr


expressionParser : Parser Expr
expressionParser =
    spaces
        |> andThen
            (\_ ->
                expression
                    |> andThen
                        (\expr ->
                            spaces
                                |> andThen
                                    (\_ ->
                                        end
                                            |> andThen
                                                (\_ ->
                                                    succeed expr
                                                )
                                    )
                        )
            )


expression : Parser Expr
expression =
    term
        |> andThen loopExpression


loopExpression : Expr -> Parser Expr
loopExpression leftExpr =
    oneOf
        [ symbol "+"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                term
                                    |> andThen
                                        (\rightExpr ->
                                            loopExpression (Add leftExpr rightExpr)
                                        )
                            )
                )
        , symbol "-"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                term
                                    |> andThen
                                        (\rightExpr ->
                                            loopExpression (Sub leftExpr rightExpr)
                                        )
                            )
                )
        , succeed leftExpr
        ]


term : Parser Expr
term =
    factor
        |> andThen loopTerm


loopTerm : Expr -> Parser Expr
loopTerm leftExpr =
    oneOf
        [ symbol "*"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                factor
                                    |> andThen
                                        (\rightExpr ->
                                            loopTerm (Mul leftExpr rightExpr)
                                        )
                            )
                )
        , symbol "/"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                factor
                                    |> andThen
                                        (\rightExpr ->
                                            loopTerm (Div leftExpr rightExpr)
                                        )
                            )
                )
        , succeed leftExpr
        ]


factor : Parser Expr
factor =
    oneOf
        [ groupedExpression
        , unary
        ]


groupedExpression : Parser Expr
groupedExpression =
    oneOf
        [ -- regular parentheses
          symbol "("
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                expression
                                    |> andThen
                                        (\expr ->
                                            spaces
                                                |> andThen
                                                    (\_ ->
                                                        symbol ")"
                                                            |> andThen
                                                                (\_ ->
                                                                    succeed (Group expr)
                                                                )
                                                    )
                                        )
                            )
                )

        -- square brackets
        , symbol "["
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                expression
                                    |> andThen
                                        (\expr ->
                                            spaces
                                                |> andThen
                                                    (\_ ->
                                                        symbol "]"
                                                            |> andThen
                                                                (\_ ->
                                                                    succeed (Group expr)
                                                                )
                                                    )
                                        )
                            )
                )

        -- curly braces
        , symbol "{"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                expression
                                    |> andThen
                                        (\expr ->
                                            spaces
                                                |> andThen
                                                    (\_ ->
                                                        symbol "}"
                                                            |> andThen
                                                                (\_ ->
                                                                    succeed (Group expr)
                                                                )
                                                    )
                                        )
                            )
                )
        ]


unary : Parser Expr
unary =
    oneOf
        [ symbol "-"
            |> andThen
                (\_ ->
                    spaces
                        |> andThen
                            (\_ ->
                                unary
                                    |> andThen
                                        (\subExpr ->
                                            succeed (Neg subExpr)
                                        )
                            )
                )
        , parseNumber
        ]


parseNumber : Parser Expr
parseNumber =
    number
        { int = Just toFloat
        , float = Just identity
        , hex = Just (\n -> toFloat n)
        , octal = Just toFloat
        , binary = Just toFloat
        }
        |> Parser.map Number


evaluateExpr : Expr -> Float
evaluateExpr expr =
    case expr of
        Number n ->
            n

        Add e1 e2 ->
            evaluateExpr e1 + evaluateExpr e2

        Sub e1 e2 ->
            evaluateExpr e1 - evaluateExpr e2

        Mul e1 e2 ->
            evaluateExpr e1 * evaluateExpr e2

        Div e1 e2 ->
            let
                denominator =
                    evaluateExpr e2
            in
            if denominator == 0 then
                1 / 0
                -- This will result in "Infinity" wtf? -> handle in calculateResult

            else
                evaluateExpr e1 / denominator

        Neg e ->
            -(evaluateExpr e)

        Group e ->
            evaluateExpr e



-- VIEW


view : Model -> Html Msg
view model =
    let
        display =
            case model.result of
                Ok val ->
                    String.fromFloat val

                Err err ->
                    "Error: " ++ err
    in
    div [ class "container" ]
        [ -- display for current input
          div [ class "display" ]
            [ text
                (if model.input == "" then
                    "0"

                 else
                    model.input
                )
            ]

        -- display for result
        , div [ class "result-display" ]
            [ text display ]

        -- buttons grid
        , div [ class "button-grid" ]
            [ calculatorButton "[" "calculator-button-light" (PressButton "[")
            , calculatorButton "]" "calculator-button-light" (PressButton "]")
            , calculatorButton "{" "calculator-button-light" (PressButton "{")
            , calculatorButton "}" "calculator-button-light" (PressButton "}")
            , calculatorButton "C" "calculator-button-light" Clear
            , calculatorButton "(" "calculator-button-light" (PressButton "(")
            , calculatorButton ")" "calculator-button-light" (PressButton ")")
            , calculatorButton "/" "calculator-button-orange" (PressButton "/")
            , calculatorButton "7" "calculator-button-dark" (PressButton "7")
            , calculatorButton "8" "calculator-button-dark" (PressButton "8")
            , calculatorButton "9" "calculator-button-dark" (PressButton "9")
            , calculatorButton "*" "calculator-button-orange" (PressButton "*")
            , calculatorButton "4" "calculator-button-dark" (PressButton "4")
            , calculatorButton "5" "calculator-button-dark" (PressButton "5")
            , calculatorButton "6" "calculator-button-dark" (PressButton "6")
            , calculatorButton "-" "calculator-button-orange" (PressButton "-")
            , calculatorButton "1" "calculator-button-dark" (PressButton "1")
            , calculatorButton "2" "calculator-button-dark" (PressButton "2")
            , calculatorButton "3" "calculator-button-dark" (PressButton "3")
            , calculatorButton "+" "calculator-button-orange" (PressButton "+")
            , calculatorButton "0" "calculator-button-dark" (PressButton "0")
            , calculatorButton "." "calculator-button-dark" (PressButton ".")
            , calculatorButton "DEL" "calculator-button-dark" Backspace
            , calculatorButton "=" "calculator-button-orange" Calculate
            ]
        ]


calculatorButton : String -> String -> Msg -> Html Msg
calculatorButton label extraClass message =
    button
        [ onClick message
        , class ("calculator-button " ++ extraClass)
        ]
        [ text label ]
