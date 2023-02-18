module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Irony


main =
    Browser.sandbox
        { init = """module X exposing (main)

import Html

main =
    Html.text "hello"
"""
        , view = view
        , update = update
        }


view : String -> Html String
view elm =
    [ header
    , textBox elm
    , typescript elm
    ]
        |> Element.column [ Element.width Element.fill ]
        |> Element.layout []


header =
    Element.text "Work in progress demo of a tool for converting Elm to React"


update : String -> String -> String
update msg _ =
    msg


typescript elm =
    let
        _ =
            Debug.log "elm" elm
    in
    case Irony.convert elm of
        Err err ->
            Element.text ("There was an error: " ++ err)

        Ok ts ->
            Element.text ts


textBox elm =
    Input.multiline
        [ Element.width Element.fill ]
        { onChange = \x -> x
        , text = elm
        , placeholder = Nothing
        , label = Input.labelAbove [] (Element.text "Type your Elm code here:")
        , spellcheck = False
        }
