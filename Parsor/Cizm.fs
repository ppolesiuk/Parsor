module Parsor.Cizm

open Parsor.Core
open Parsor.Combinators
open Parsor.Primitives

let cComment<'State> : Parsor<char, 'State, unit> =
    skipChar '/' .>>
    (
        (skipChar '/' <!> fun() -> skipMany (notFollowedBy (skipChar '\n') .>> getToken) >>. skipChar '\n') <|>
        (skipChar '*' <!> fun() -> skipMany (notFollowedBy (skipString "*/") .>> getToken) >>. skipString "*/")
    ) <??> "comment"

let cWhiteSpace<'State> : Parsor<char, 'State, unit> =
    (skipMany (whiteSpace <|> cComment)) <??> "white space"

let cDouble =
    parsor{
        let! sign = parseSign
        let! c = digit
        let! ipart = foldl (fun x c -> x*10.0 + float(int c - int '0')) (float <| int c - int '0') digit
        do! skipChar '.'
        let! fpart = foldl (fun (m, x) c -> (m*0.1, x + m * float(int c - int '0'))) (0.1, ipart) digit |>> snd
        return! parsor{
            let! e = tryParse <| (skipChar 'e' <|> skipChar 'E')
            match e with
            | None -> return fpart * float sign
            | Some _ ->
                let! exponent = pnumber
                return pown 10. exponent * fpart * float sign
        }
    } <??> "double constant"
