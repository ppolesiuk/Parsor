module Parsor.Cizm

open Parsor.Core
open Parsor.Combinators
open Parsor.Primitives

let cComment : Parsor<char, unit> =
    skipChar '/' .>>
    (
        (skipChar '/' <!> fun() -> skipMany (notFollowedBy(skipChar '\n') "new line" .>> getToken) >>. skipChar '\n') ^|
        (skipChar '*' <!> fun() -> skipMany (notFollowedBy(skipString "*/") "close comment sequence" .>> getToken) >>. skipString "*/") ^|
        fatalError "Invalid comment startup sequence."
    )

let cWhiteSpace : Parsor<char, unit> =
    let rec wsParse arg =
        (
        whiteSpace .>>
        (
            (cComment <!> fun() -> wsParse) ^|
            parsor.Return ()
        )) arg
    in
        wsParse


let cDouble =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! ipart = foldl (float <| int c - int '0') (fun x c -> x*10.0 + float(int c - int '0')) digit
        do! skipChar '.'
        let! fpart = foldl (0.1, ipart) (fun (m, x) c -> (m*0.1, x + m * float(int c - int '0'))) digit |>> snd
        return! parsor{
            let! e = tryParse <| (skipChar 'e' <|> skipChar 'E')
            match e with
            | None -> return fpart * float sign
            | Some _ ->
                let! exponent = pnumber
                return pown 10. exponent * fpart * float sign
        }
    }
