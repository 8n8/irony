module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import Irony


suite : Test
suite =
    test "hello world app" <|
        \_ ->
            Expect.equal (Irony.convert elm) react

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
