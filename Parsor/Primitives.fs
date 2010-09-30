module Parsor.Primitives

open Parsor.Combinators
open Parsor.Core
open System

let private charToString c =
    (if c < ' ' then "" else "'" + c.ToString() + "' ") + "(0x" + (int c).ToString("X") + ")"

let eof : Parsor<'Input, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            (input, ())
        else
            raise <| FatalError(input.Position, "Unexpected end of file")

let warning message : Parsor<'Input, unit> =
    fun (env, input) ->
        env.Error(input.Position, message);
        (input, ())

let error message : Parsor<'Input, unit> =
    fun (env, input) ->
        env.Error(input.Position, message);
        (input, ())

let fatalError message : Parsor<'Input, 'Output> =
    fun (env, input) ->
        raise <| FatalError(input.Position, message)

let whiteChar : Parsor<char, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected white char")
        else if input.Head |> Char.IsWhiteSpace then
            (input.Tail, ())
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected white char")

let whiteSpace : Parsor<char, unit> = skipMany whiteChar
let whiteSpace1 : Parsor<char, unit> = skipMany1 whiteChar

let letter : Parsor<char, char> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected letter")
        else if input.Head |> Char.IsLetter then
            (input.Tail, input.Head)
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected letter")
let digit : Parsor<char, char> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected digit")
        else if input.Head |> Char.IsDigit then
            (input.Tail, input.Head)
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected digit")

let parseChar pred name : Parsor<char, char> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + name)
        else if input.Head |> pred then
            (input.Tail, input.Head)
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected " + name)

let skipCharPred pred name : Parsor<char, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + name)
        else if input.Head |> pred then
            (input.Tail, ())
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected " + name)

let skipChar c : Parsor<char, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + charToString c)
        else if input.Head = c then
            (input.Tail, ())
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected " + charToString c)

let skipCharIgnoreCase c : Parsor<char, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + charToString c)
        else if Char.ToUpper input.Head = Char.ToUpper c then
            (input.Tail, ())
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected " + charToString c)

let pChar c : Parsor<char, char> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + charToString c)
        else if input.Head = c then
            (input.Tail, input.Head)
        else
            raise <| FatalError(input.Position, 
                "Unexpected character : " + charToString input.Head + ", expected " + charToString c)

let skipToken token : Parsor<'Input, unit> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream, expected " + token.ToString())
        else if input.Head = token then
            (input.Tail, ())
        else
            raise <| FatalError(input.Position, 
                "Unexpected token : " + input.Head.ToString() + ", expected " + token.ToString())

let skipLine : Parsor<char, unit> =
    let rec skip (env, input : IParsorInput<char>) =
        if input.IsEmpty then
            (input, ())
        else if input.Head = '\n' then
            (input.Tail, ())
        else
            skip (env, input.Tail)
    in
        skip

let getToken : Parsor<'Input, 'Input> =
    fun (env, input) ->
        if input.IsEmpty then
            raise <| FatalError(input.Position, "Unexpected end of stream")
        else 
            (input.Tail, input.Head)

let skipString str : Parsor<char, unit> =
    parsor{
        for c in str do do! skipChar c
    }
let skipStringIgnoreCase str : Parsor<char, unit> =
    parsor{
        for c in str do do! skipCharIgnoreCase c
    }

let manyChars p = foldl "" (fun s c -> s + c.ToString()) p
let many1Chars p = 
    parsor{
        let! x = p
        let! r = foldl "" (fun s c -> s + c.ToString()) p
        return x.ToString() + r
    }

let parseString p str =
    parse p (Parsor.Environment.ConsoleEnvironment()) (Parsor.Input.StringInput str)

let internal parseSign : Parsor<char, int> =
    (skipChar '+' <!> fun() -> parsor.Return 1) ^|
    (skipChar '-' <!> fun() -> parsor.Return -1) ^|
    parsor.Return 1

let pnumber =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! result = foldl (int c - int '0') (fun x c -> x*10 + int c - int '0') digit
        return sign * result
    }

let pfloat =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! ipart = foldl (float <| int c - int '0') (fun x c -> x*10.0 + float(int c - int '0')) digit
        let! dot = tryParse <| skipChar '.'
        let! fpart = 
            parsor{
                match dot with
                | None -> return ipart
                | Some _ ->
                    let! (_, x) = foldl (0.1, ipart) (fun (m, x) c -> (m*0.1, x + m * float(int c - int '0'))) digit in
                    return x
            }
        return! parsor{
            let! e = tryParse <| (skipChar 'e' <|> skipChar 'E')
            match e with
            | None -> return fpart * float sign
            | Some _ ->
                let! exponent = pnumber
                return pown 10. exponent * fpart * float sign
        }
    }
