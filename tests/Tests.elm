module Tests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Irony
import Test exposing (..)


suite : Test
suite =
    test "hello world app" <|
        \_ ->
            Expect.equal (Irony.convert elm) (Ok react)


elm : String
elm =
    """module Main exposing (main)

import Html


main =
    Html.text "hi"
"""


react : String
react =
    """export function Main() {
  return <h1>Hello</h1>;
}"""
