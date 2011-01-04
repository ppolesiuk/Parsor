module Parsor.Primitives

open Parsor.Combinators
open Parsor.Core
open System

let private charToString c =
    (if c < ' ' then "" else "'" + c.ToString() + "' ") + "(0x" + (int c).ToString("X") + ")"

let eof<'Input, 'State> : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.Return ()
            else
                arg.ReturnErrorFE (lazy "Expected end of file") ()
    ), lazy "end of file")

let getPos<'Input, 'State> : Parsor<'Input, 'State, IPosition> =
    ((
        fun arg ->
            arg.Return arg.Input.Position
    ), lazy "nothing")

let skipGetPos (p : Parsor<'Input, 'State, 'A>) : Parsor<'Input, 'State, IPosition> =
    ((
        fun arg ->
            match fst p arg with
            | ParseOk(stat', msg', _) -> ParseOk(stat', msg', arg.Input.Position)
            | ParseConsumedOk(inp', stat', msg', _) -> ParseConsumedOk(inp', stat', msg', arg.Input.Position)
            | ParseError(stat', msg', _) -> ParseError(stat', msg', arg.Input.Position)
            | ParseConsumedError(inp', stat', msg', _) -> ParseConsumedError(inp', stat', msg', arg.Input.Position)
            | ParseFatalError msg' -> ParseError(arg.State, msg', arg.Input.Position)
            | ParseConsumedFatalError msg' -> ParseConsumedFatalError msg'
    ), snd p)

let withPos (p : Parsor<'Input, 'State, 'A>) : Parsor<'Input, 'State, 'A * IPosition> =
    ((
        fun arg ->
            match fst p arg with
            | ParseOk(stat', msg', v) -> ParseOk(stat', msg', (v, arg.Input.Position))
            | ParseConsumedOk(inp', stat', msg', v) -> ParseConsumedOk(inp', stat', msg', (v, arg.Input.Position))
            | ParseError(stat', msg', v) -> ParseError(stat', msg', (v, arg.Input.Position))
            | ParseConsumedError(inp', stat', msg', v) -> ParseConsumedError(inp', stat', msg', (v, arg.Input.Position))
            | ParseFatalError msg' -> ParseFatalError msg'
            | ParseConsumedFatalError msg' -> ParseConsumedFatalError msg'
    ), snd p)

let message message : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            arg.ReturnM ((MsgMessage(lazy message))::arg.Messages) ()
    ), lazy "nothing")

let warning message : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            arg.ReturnM (MsgWarning(arg.Input.Position, lazy message)::arg.Messages) ()
    ), lazy "nothing")

let error message : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            arg.ReturnErrorE (lazy message) ()
    ), lazy "nothing")

let fatalError message : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            arg.ReturnFatalErrorFE (lazy message)
    ), lazy "nothing")

let whiteChar<'State> : Parsor<char, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy "Unexpected end of stream, expected white char") ()
            else if arg.Input.Head |> Char.IsWhiteSpace then
                arg.ReturnConsumedI arg.Input.Tail ()
            else
                arg.ReturnErrorE (lazy ("Unexpected character : " + charToString arg.Input.Head + ", expected white char")) ()
    ), lazy "white char")

let whiteSpace<'State> : Parsor<char, 'State, unit> = (skipMany whiteChar) <?> "white space"
let whiteSpace1<'State> : Parsor<char, 'State, unit> = (skipMany1 whiteChar) <?> "white space"

let letter<'State> : Parsor<char, 'State, char> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnFatalErrorE <| lazy "Unexpected end of stream, expected letter"
            else if arg.Input.Head |> Char.IsLetter then
                arg.ReturnConsumedI arg.Input.Tail arg.Input.Head
            else
                arg.ReturnFatalErrorE <| lazy( "Unexpected character : " + charToString arg.Input.Head + ", expected letter" )
    ), lazy "letter")
let digit<'State> : Parsor<char, 'State, char> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnFatalErrorE <| lazy "Unexpected end of stream, expected digit"
            else if arg.Input.Head |> Char.IsDigit then
                arg.ReturnConsumedI arg.Input.Tail arg.Input.Head
            else
                arg.ReturnFatalErrorE <| lazy( "Unexpected character : " + charToString arg.Input.Head + ", expected digit" )
    ), lazy "digit")
let parseChar pred name : Parsor<char, 'State, char> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnFatalErrorE <| lazy( "Unexpected end of stream, expected " + name )
            else if arg.Input.Head |> pred then
                arg.ReturnConsumedI arg.Input.Tail arg.Input.Head
            else
                arg.ReturnFatalErrorE <| lazy( "Unexpected character : " + charToString arg.Input.Head + ", expected " + name )
    ), lazy name)
let skipCharPred pred name : Parsor<char, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy ("Unexpected end of stream, expected " + name)) ()
            else if arg.Input.Head |> pred then
                arg.ReturnConsumedI arg.Input.Tail ()
            else
                arg.ReturnErrorE (lazy ("Unexpected character : " + charToString arg.Input.Head + ", expected " + name)) ()
    ), lazy name)
let skipChar c : Parsor<char, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy ("Unexpected end of stream, expected " + charToString c)) ()
            else if arg.Input.Head = c then
                arg.ReturnConsumedI arg.Input.Tail ()
            else
                arg.ReturnErrorE (lazy ("Unexpected character : " + charToString arg.Input.Head + ", expected " + charToString c)) ()
    ), lazy (charToString c))
let skipCharIgnoreCase c : Parsor<char, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy ("Unexpected end of stream, expected " + charToString c)) ()
            else if Char.ToUpper arg.Input.Head = Char.ToUpper c then
                arg.ReturnConsumedI arg.Input.Tail ()
            else
                arg.ReturnErrorE (lazy ("Unexpected character : " + charToString arg.Input.Head + ", expected " + charToString c)) ()
    ), lazy (charToString c))
let pChar c : Parsor<char, 'State, char> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy ("Unexpected end of stream, expected " + charToString c)) c
            else if arg.Input.Head = c then
                arg.ReturnConsumedI arg.Input.Tail c
            else
                arg.ReturnErrorE (lazy ("Unexpected character : " + charToString arg.Input.Head + ", expected " + charToString c)) c
    ), lazy (charToString c))
let skipToken token : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnErrorE (lazy ("Unexpected end of stream, expected " + token.ToString())) ()
            else if arg.Input.Head = token then
                arg.ReturnConsumedI arg.Input.Tail ()
            else
                arg.ReturnErrorE (lazy ("Unexpected token : " + arg.Input.Head.ToString() + ", expected " + token.ToString())) ()
    ), lazy (token.ToString()))
let skipLine<'State> : Parsor<char, 'State, unit> =
    let rec skip (arg : ParsorArg<char, 'State>) =
        if arg.Input.IsEmpty then
            arg.Return ()
        else if arg.Input.Head = '\n' then
            arg.ReturnConsumedI arg.Input.Tail ()
        else
            skip (ParsorArg(arg.Input.Tail, arg.State, arg.Messages, arg.Ok, true))
    in (skip, lazy "line")
let getToken<'Input,'State> : Parsor<'Input, 'State, 'Input> =
    ((
        fun arg ->
            if arg.Input.IsEmpty then
                arg.ReturnFatalErrorE <| lazy "Unexpected end of stream"
            else 
                arg.ReturnConsumedI arg.Input.Tail arg.Input.Head
    ), lazy "token")
let skipString str : Parsor<char, 'State, unit> =
    parsor{
        for c in str do do! skipChar c
    } <??> ("string \"" + str + "\"")

let skipStringIgnoreCase str : Parsor<char, 'State, unit> =
    parsor{
        for c in str do do! skipCharIgnoreCase c
    } <??> ("string \"" + str + "\"")

let manyChars p = foldl (fun s c -> s + c.ToString()) "" p
let many1Chars p = 
    parsor{
        let! x = p
        let! r = foldl (fun s c -> s + c.ToString()) "" p
        return x.ToString() + r
    }

let parseString p stat0 cons str =
    parse p stat0 cons (Parsor.Input.StringInput str)

let internal parseSign<'State> : Parsor<char, 'State, int> =
    (skipChar '+' >>. parsor.Return 1) <|>
    (skipChar '-' >>. parsor.Return -1) <|>
    parsor.Return 1

let pnumber<'State> =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! result = foldl (fun x c -> x*10 + int c - int '0') (int c - int '0') digit
        return sign * result
    }

let pfloat : Parsor<char, unit, float> =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! ipart = foldl (fun x c -> x*10.0 + float(int c - int '0')) (float <| int c - int '0') digit
        let! dot = tryParse <| skipChar '.'
        let! fpart = 
            parsor{
                match dot with
                | None -> return ipart
                | Some _ ->
                    let! (_, x) = foldl (fun (m, x) c -> (m*0.1, x + m * float(int c - int '0'))) (0.1, ipart) digit in
                    return x
            }
        let! e = tryParse <| (skipChar 'e' <|> skipChar 'E')
        match e with
        | None -> return fpart * float sign
        | Some _ ->
            let! exponent = pnumber
            return pown 10. exponent * fpart * float sign
    } <??> "float"
