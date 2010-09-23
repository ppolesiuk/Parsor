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