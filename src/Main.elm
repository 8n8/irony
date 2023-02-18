module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Irony


main =
    Browser.sandbox
        { init = """module Main exposing (main)

import Html

main =
    Html.text "hello\""""
        , view = view
        , update = update
        }


view : String -> Html String
view elm =
    [ header
    , Element.paragraph [] [ Element.text "So far it handles simple imports, exports, function declarations, function calls and string literals." ]
    , textBox elm
    , typescript elm
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing 24
            , Element.padding 8
            ]
        |> Element.layout [ Font.family [ Font.sansSerif ] ]


header =
    [ Element.text "Convert Elm to Typescript and React"
        |> Element.el
            [ Font.size 32
            ]
    , Element.text "A work-in-progress demo"
    ]
        |> Element.column [ Element.spacing 8 ]


update : String -> String -> String
update msg _ =
    msg


typescript elm =
    case Irony.convert elm of
        Err err ->
            Element.text ("There was an error: " ++ err)

        Ok ts ->
            [ Element.text "Here is the corresponding Typescript and React code:"
            , Element.text ts
                |> Element.el [ Font.family [ Font.monospace ] ]
            ]
                |> Element.column [ Element.spacing 16 ]


textBox elm =
    Input.multiline
        [ Element.width Element.fill
        , Font.family [ Font.monospace ]
        ]
        { onChange = \x -> x
        , text = elm
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.family [ Font.sansSerif ] ] (Element.text "Type your Elm code here:")
        , spellcheck = False
        }
