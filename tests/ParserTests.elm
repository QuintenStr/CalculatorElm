module ParserTests exposing (suite)

import Expect
import Main exposing (calculateResult)
import Test exposing (..)


suite : Test
suite =
    describe "Parser Tests"
        [ -- Basic Arithmetic
          test "Addition" <| \_ -> Expect.equal (calculateResult "2+3") (Ok 5)
        , test "Subtraction" <| \_ -> Expect.equal (calculateResult "7-4") (Ok 3)
        , test "Multiplication" <| \_ -> Expect.equal (calculateResult "6*3") (Ok 18)
        , test "Division" <| \_ -> Expect.equal (calculateResult "8/2") (Ok 4)

        -- Precedence
        , test "Mixed Operations with Precedence" <|
            \_ -> Expect.equal (calculateResult "2+3*4") (Ok 14)
        , test "Parentheses Change Precedence" <|
            \_ -> Expect.equal (calculateResult "(2+3)*4") (Ok 20)
        , test "Complex Nesting" <| \_ -> Expect.equal (calculateResult "2*(3+(4*5))") (Ok 46)
        , test "Division with Precedence" <| \_ -> Expect.equal (calculateResult "16/4+2") (Ok 6)

        -- Negation
        , test "Negative Numbers" <| \_ -> Expect.equal (calculateResult "-3+5") (Ok 2)
        , test "Negative Inside Parentheses" <|
            \_ -> Expect.equal (calculateResult "(-3+5)*2") (Ok 4)

        -- Grouping
        , test "Square Brackets" <| \_ -> Expect.equal (calculateResult "[2+3]*4") (Ok 20)
        , test "Curly Braces" <| \_ -> Expect.equal (calculateResult "{2+3}*4") (Ok 20)
        , test "Mixed Grouping" <| \_ -> Expect.equal (calculateResult "(2+{3*[4*(5)]})") (Ok 62)

        -- Floating-Point Numbers
        , test "Simple Float" <| \_ -> Expect.equal (calculateResult "3.14+2") (Ok 5.14)
        , test "Complex Float" <| \_ -> Expect.equal (calculateResult "1.5*2.5+0.5") (Ok 4.25)

        -- Different Number Systems
        , test "Hexadecimal" <| \_ -> Expect.equal (calculateResult "0xA+5") (Ok 15)
        , test "Octal" <| \_ -> Expect.equal (calculateResult "0o10+2") (Ok 10)
        , test "Binary" <| \_ -> Expect.equal (calculateResult "0b101+3") (Ok 8)

        -- Errors
        , test "Invalid Syntax" <| \_ -> Expect.equal (calculateResult "2++3") (Err "Invalid expression")
        , test "Mismatched Grouping" <| \_ -> Expect.equal (calculateResult "((2+3)") (Err "Invalid expression")
        , test "Unsupported Characters" <| \_ -> Expect.equal (calculateResult "2+3a") (Err "Invalid expression")

        -- Edge Cases
        , test "Empty Input" <| \_ -> Expect.equal (calculateResult "") (Ok 0)
        , test "Only Negation" <| \_ -> Expect.equal (calculateResult "-") (Err "Invalid expression")
        , test "Division by Zero" <|
            \_ ->
                case calculateResult "4/0" of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Expected error for division by zero"
        ]
