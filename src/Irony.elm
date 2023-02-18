module Irony exposing (convert)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))


convert : String -> Result String String
convert elm =
    case parse elm of
        Err _ ->
            Err "failed"

        Ok ast ->
            Ok (toReact ast)


parse : String -> Result String File
parse elm =
    case Elm.Parser.parse elm of
        Err _ ->
            Err "parsing failed"

        Ok rawFile ->
            Ok (Elm.Processing.process Elm.Processing.init rawFile)


toReact : File -> String
toReact { declarations, moduleDefinition, imports } =
    [ importsToReact imports
    , String.join "\n\n" (List.map declarationToReact declarations)
    , "\n\n"
    , exportsToReact (unwrap moduleDefinition)
    , "\n"
    ]
        |> String.concat


importsToReact imports =
    case imports of
        [] ->
            ""

        oneOrMore ->
            [ String.join "\n" (List.map (unwrap >> importToReact) imports)
            , "\n\n"
            ]
                |> String.concat


importToReact { moduleName } =
    [ "import * as "
    , String.join "." (unwrap moduleName)
    , " from \"./"
    , String.join "/" (unwrap moduleName)
    , "\""
    , ";"
    ]
        |> String.concat


exportsToReact : Module -> String
exportsToReact module_ =
    case module_ of
        PortModule _ ->
            Debug.todo "PortModule not implemented yet"

        EffectModule _ ->
            Debug.todo "EffectModule not implemented yet"

        NormalModule { exposingList } ->
            case unwrap exposingList of
                Explicit [] ->
                    ""

                All _ ->
                    Debug.todo "Exposing all not implemented yet"

                Explicit oneOrMore ->
                    [ "export {"
                    , String.join ", " (List.map (unwrap >> exportToReact) oneOrMore)
                    , "};"
                    ]
                        |> String.concat


exportToReact : TopLevelExpose -> String
exportToReact expose =
    case expose of
        FunctionExpose name ->
            name

        InfixExpose _ ->
            Debug.todo "InfixExpose not implemented"

        TypeOrAliasExpose _ ->
            Debug.todo "TypeOrAliasExpose not implemented"

        TypeExpose _ ->
            Debug.todo "TypeExpose not implemented"


declarationToReact : Node Declaration -> String
declarationToReact (Node _ declaration) =
    case declaration of
        FunctionDeclaration function ->
            functionToReact function

        AliasDeclaration _ ->
            Debug.todo "Alias declaration not implemented"

        CustomTypeDeclaration _ ->
            Debug.todo "Custom type declaration not implemented"

        PortDeclaration _ ->
            Debug.todo "Port declaration not implemented"

        InfixDeclaration _ ->
            Debug.todo "Infix declaration not implemented"

        Destructuring _ _ ->
            Debug.todo "Destructuring not implemented"


unwrap : Node a -> a
unwrap (Node _ a) =
    a


functionToReact : Function -> String
functionToReact { declaration } =
    functionDeclarationToReact (unwrap declaration)


functionDeclarationToReact : FunctionImplementation -> String
functionDeclarationToReact { name, expression } =
    [ "function "
    , unwrap name
    , "() {\n  return "
    , expressionToReact (unwrap expression)
    , ";\n}"
    ]
        |> String.concat


expressionToReact : Expression -> String
expressionToReact expression =
    case expression of
        FunctionOrValue moduleName functionName ->
            String.join "." (moduleName ++ [ functionName ])

        IfBlock _ _ _ ->
            Debug.todo "IfBlock not implemented"

        PrefixOperator _ ->
            Debug.todo "PrefixOperator not implemented"

        Operator _ ->
            Debug.todo "Operator not implemented"

        Integer _ ->
            Debug.todo "Integer not implemented"

        Hex _ ->
            Debug.todo "Hex not implemented"

        Floatable _ ->
            Debug.todo "Floatable not implemented"

        Negation _ ->
            Debug.todo "Negation not implemented"

        Literal literal ->
            String.concat [ "\"", literal, "\"" ]

        CharLiteral _ ->
            Debug.todo "CharLiteral not implemented"

        TupledExpression _ ->
            Debug.todo "TupledExpression not implemented"

        LetExpression _ ->
            Debug.todo "LetExpression not implemented"

        CaseExpression _ ->
            Debug.todo "CaseExpression not implemented"

        LambdaExpression _ ->
            Debug.todo "LambdaExpression not implemented"

        RecordExpr _ ->
            Debug.todo "RecordExpr not implemented"

        ListExpr _ ->
            Debug.todo "ListExpr not implemented"

        RecordAccess _ _ ->
            Debug.todo "RecordAccess not implemented"

        RecordAccessFunction _ ->
            Debug.todo "RecordAccessFunction not implemented"

        RecordUpdateExpression _ _ ->
            Debug.todo "RecordUpdateExpression not implemented"

        GLSLExpression _ ->
            Debug.todo "GLSLExpression not implemented"

        ParenthesizedExpression parenthesized ->
            [ "("
            , expressionToReact (unwrap parenthesized)
            , ")"
            ]
                |> String.concat

        UnitExpr ->
            Debug.todo "UnitExpr not implemented"

        Application app ->
            List.map unwrap app
                |> List.map expressionToReact
                |> applicationToReact

        OperatorApplication _ _ _ _ ->
            Debug.todo "OperatorApplication not implemented"


applicationToReact : List String -> String
applicationToReact items =
    case items of
        [] ->
            ""

        top :: tail ->
            [ top
            , "("
            , String.join ", " tail
            , ")"
            ]
                |> String.concat
