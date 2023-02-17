module Irony exposing (convert)


convert : String -> Result String String
convert elm =
    """export function Main() {
  return <h1>Hello</h1>;
}"""
        |> Ok
