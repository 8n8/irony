module Irony exposing (convert)

import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))


convert : String -> Result String String
convert elm =
    case parse elm of
        Err err ->
            Err err

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
            "PortModule NOT IMPLEMENTED YET"

        EffectModule _ ->
            "EffectModule NOT IMPLEMENTED YET"

        NormalModule { exposingList } ->
            case unwrap exposingList of
                Explicit [] ->
                    ""

                All _ ->
                    "Exposing all NOT IMPLEMENTED YET"

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
            "InfixExpose NOT IMPLEMENTED"

        TypeOrAliasExpose _ ->
            "TypeOrAliasExpose NOT IMPLEMENTED"

        TypeExpose _ ->
            "TypeExpose NOT IMPLEMENTED"


declarationToReact : Node Declaration -> String
declarationToReact (Node _ declaration) =
    case declaration of
        FunctionDeclaration function ->
            functionToReact function

        AliasDeclaration _ ->
            "Alias declaration NOT IMPLEMENTED"

        CustomTypeDeclaration _ ->
            "Custom type declaration NOT IMPLEMENTED"

        PortDeclaration _ ->
            "Port declaration NOT IMPLEMENTED"

        InfixDeclaration _ ->
            "Infix declaration NOT IMPLEMENTED"

        Destructuring _ _ ->
            "Destructuring NOT IMPLEMENTED"


unwrap : Node a -> a
unwrap (Node _ a) =
    a


functionToReact : Function -> String
functionToReact { declaration } =
    functionDeclarationToReact (unwrap declaration)


functionDeclarationToReact : FunctionImplementation -> String
functionDeclarationToReact { name, expression, arguments } =
    if List.isEmpty arguments then
        [ "var "
        , unwrap name
        , " =\n"
        , "  "
        , expressionToReact (unwrap expression)
        , ";"
        ]
            |> String.concat

    else
        [ "function "
        , unwrap name
        , "("
        , String.join ", " (List.map (unwrap >> patternToReact) arguments)
        , ") {\n  return "
        , expressionToReact (unwrap expression)
        , ";\n}"
        ]
            |> String.concat


patternToReact : Pattern -> String
patternToReact pattern =
    case pattern of
        VarPattern name ->
            name

        AllPattern ->
            "AllPattern NOT IMPLEMENTED"

        UnitPattern ->
            "UnitPattern NOT IMPLEMENTED"

        CharPattern _ ->
            "CharPattern NOT IMPLEMENTED"

        StringPattern _ ->
            "StringPattern NOT IMPLEMENTED"

        IntPattern _ ->
            "IntPattern NOT IMPLEMENTED"

        HexPattern _ ->
            "HexPattern NOT IMPLEMENTED"

        FloatPattern _ ->
            "FloatPattern NOT IMPLEMENTED"

        TuplePattern _ ->
            "TuplePattern NOT IMPLEMENTED"

        RecordPattern _ ->
            "RecordPattern NOT IMPLEMENTED"

        UnConsPattern _ _ ->
            "UnConsPattern NOT IMPLEMENTED"

        ListPattern _ ->
            "ListPattern NOT IMPLEMENTED"

        NamedPattern _ _ ->
            "NamedPattern NOT IMPLEMENTED"

        AsPattern _ _ ->
            "AsPattern NOT IMPLEMENTED"

        ParenthesizedPattern _ ->
            "ParenthesizedPattern NOT IMPLEMENTED"


expressionToReact : Expression -> String
expressionToReact expression =
    case expression of
        FunctionOrValue moduleName functionName ->
            String.join "." (moduleName ++ [ functionName ])

        IfBlock _ _ _ ->
            "IfBlock NOT IMPLEMENTED"

        PrefixOperator _ ->
            "PrefixOperator NOT IMPLEMENTED"

        Operator _ ->
            "Operator NOT IMPLEMENTED"

        Integer _ ->
            "Integer NOT IMPLEMENTED"

        Hex _ ->
            "Hex NOT IMPLEMENTED"

        Floatable _ ->
            "Floatable NOT IMPLEMENTED"

        Negation _ ->
            "Negation NOT IMPLEMENTED"

        Literal literal ->
            String.concat [ "\"", literal, "\"" ]

        CharLiteral _ ->
            "CharLiteral NOT IMPLEMENTED"

        TupledExpression _ ->
            "TupledExpression NOT IMPLEMENTED"

        LetExpression _ ->
            "LetExpression NOT IMPLEMENTED"

        CaseExpression _ ->
            "CaseExpression NOT IMPLEMENTED"

        LambdaExpression _ ->
            "LambdaExpression NOT IMPLEMENTED"

        RecordExpr _ ->
            "RecordExpr NOT IMPLEMENTED"

        ListExpr _ ->
            "ListExpr NOT IMPLEMENTED"

        RecordAccess _ _ ->
            "RecordAccess NOT IMPLEMENTED"

        RecordAccessFunction _ ->
            "RecordAccessFunction NOT IMPLEMENTED"

        RecordUpdateExpression _ _ ->
            "RecordUpdateExpression NOT IMPLEMENTED"

        GLSLExpression _ ->
            "GLSLExpression NOT IMPLEMENTED"

        ParenthesizedExpression parenthesized ->
            [ "("
            , expressionToReact (unwrap parenthesized)
            , ")"
            ]
                |> String.concat

        UnitExpr ->
            "UnitExpr NOT IMPLEMENTED"

        Application app ->
            List.map unwrap app
                |> List.map expressionToReact
                |> applicationToReact

        OperatorApplication _ _ _ _ ->
            "OperatorApplication NOT IMPLEMENTED"


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
