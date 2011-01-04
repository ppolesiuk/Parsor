
open Parsor.Core
open Parsor.Combinators
open Parsor.Primitives
open Parsor.Expresion

let ws = whiteSpace
let expr = ExpresionParsor<char, unit, float>()
let func str = skipString str .>> notFollowedBy (letter <|> digit) >>. ws

let atom =
    (
        (skipChar '(' <!> fun () -> ws >>. expr.Parsor .>> skipChar ')' .>> ws) <|>
        (pfloat .>> ws)
    ) <??> "atom"

do
    expr.Atom <- atom
    expr.AddInfixOperator (skipChar '+' >>. ws >>. parsor.Return (+)) LeftAssoc 4
    expr.AddInfixOperator (skipChar '-' >>. ws >>. parsor.Return (-)) LeftAssoc 4
    expr.AddInfixOperator (skipChar '*' >>. ws >>. parsor.Return (*)) LeftAssoc 5
    expr.AddInfixOperator (skipChar '/' >>. ws >>. parsor.Return (/)) LeftAssoc 5
    expr.AddInfixOperator (skipChar '^' >>. ws >>. parsor.Return (fun x y -> System.Math.Pow(x,y))) RightAssoc 6
    expr.AddPrefixOperator (func "sin" >>. parsor.Return sin) 7
    expr.AddPrefixOperator (func "cos" >>. parsor.Return cos) 7
    expr.AddPrefixOperator (func "log" >>. parsor.Return log) 7
    expr.AddPrefixOperator (func "exp" >>. parsor.Return exp) 7

let allParsor = ws >>. expr.Parsor .>> eof

let main =
    while true do
        match System.Console.ReadLine() |> parseString allParsor () (OutputConsole()) with
        | Success x ->
            System.Console.WriteLine x
        | Error -> ()
