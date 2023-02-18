module Tests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Irony
import Test exposing (..)


suite : Test
suite =
    List.map oneTest cases |> describe "unit tests"


oneTest : Case -> Test
oneTest { description, elm, react } =
    test description <|
        \_ ->
            Expect.equal (Irony.convert elm) (Ok react)


type alias Case =
    { elm : String
    , description : String
    , react : String
    }


cases : List Case
cases =
    [ { elm = """module Main exposing (main)

import Html


main =
    Html.h1 (Html.text "hi")
"""
      , react = """import * as Html from "./Html";

var main = Html.h1((Html.text("hi")));

export {main};
"""
      , description = "hello world"
      }
    , { elm = """module X exposing (x)


x a =
    a
"""
      , react = """function x(a) {
  return a;
}

export {x};
"""
      , description = "function declaration with parameters"
      }
    ]
