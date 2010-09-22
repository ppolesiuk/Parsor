module Parsor.Tokenizor

open Parsor.Core
open Parsor.Input
open Parsor.Primitives

let rec tokenize (p : Parsor<'A, 'B>) env inp : IParsorInput<'B> =
    try
        eof(env, inp) |> ignore;
        LazyInput(inp.Position, None) :> IParsorInput<'B>
    with
    | FatalError _ ->
        let (rInp, value) = p(env, inp) in
            LazyInput(inp.Position, Some(value, fun () -> tokenize p env rInp)) :> IParsorInput<'B>
