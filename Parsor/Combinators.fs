module Parsor.Combinators

open Parsor.Core
open Parsor.Environment

let skip (p : Parsor<'Input, 'T>) : Parsor<'Input, unit> =
    fun (env, inp) ->
        let (rInp, _) = p(env, inp) in (rInp, ())

let skipMany(p : Parsor<'Input, 'T>) : Parsor<'Input, unit> =
    let rec resultParsor(env, inp) =
        try
            let (rInp, _) = p(env, inp) in
                resultParsor(env, rInp)
        with
        | FatalError _ -> (inp, ())
    in
        resultParsor

let skipMany1 p =
    parsor{
        do! skip p
        return! skipMany p
    }

let tryParse (p : Parsor<'Input, 'T>) : Parsor<'Input, 'T option> =
    fun (env, inp) ->
        try
            let (rInp, value) = p(env, inp) in (rInp, Some value)
        with
        | FatalError _ -> (inp, None)

let rec foldl stat func (p : Parsor<'Input, 'T>) : Parsor<'Input, 'S> =
    fun (env, inp) ->
        try
            let (rInp, value) = p(env, inp) in
                (foldl (func stat value) func p) (env, rInp)
        with
        | FatalError _ -> (inp, stat)
let rec foldr func stat (p : Parsor<'Input, 'T>) : Parsor<'Input, 'S> =
    fun (env, inp) ->
        try
            let (rInp, value) = p(env, inp) in
            let (rrInp, rstat) = (foldr func stat p) (env, rInp) in
                (rrInp, func value rstat)
        with
        | FatalError _ -> (inp, stat)

let rec multiOption(opt : Parsor<'Input, 'T> seq) : Parsor<'Input, 'T> =
    fun (env, inp) ->
        if Seq.isEmpty opt then
            raise <| FatalError(inp.Position, "Syntax error")
        else
            try
                (Seq.head opt)(env, inp)
            with
            | FatalError _ -> multiOption(Seq.skip 1 opt) (env, inp)

let condition (cond : Parsor<'Input, 'A>) (p : 'A -> Parsor<'Input, 'B>) =
    fun (env, inp) ->
        let cRes =
            try
                Some <| cond(env, inp)
            with
            | FatalError _ -> None
        match cRes with
        | None -> None
        | Some(rInp, value) ->
            Some <| p value (env, rInp)

let rec multiOptionC opt : Parsor<'Input, 'T> =
    fun (env, inp) ->
        if Seq.isEmpty opt then
            raise <| FatalError(inp.Position, "Syntax error")
        else
            match (Seq.head opt)(env, inp) with
            | None ->
                multiOptionC (Seq.skip 1 opt) (env, inp)
            | Some x -> x
let lookAhead (p : Parsor<'Input, 'Output>) : Parsor<'Input, 'Output> =
    fun (env, inp) ->
        let (_, v) = p (env, inp) in (inp, v)
let followedBy (p : Parsor<'Input, 'Output>) n : Parsor<'Input, unit> =
    fun (env, inp) ->
        try
            p(env, inp) |> ignore
            (inp, ())
        with
        | FatalError _ ->
            raise <| FatalError(inp.Position, "Expected : " + n)
let notFollowedBy (p : Parsor<'Input, 'Output>) n : Parsor<'Input, unit> =
    fun (env, inp) ->
        if
            try
                p(env, inp) |> ignore
                true
            with
            | FatalError _ ->
                false
        then
            raise <| FatalError(inp.Position, "Unexpected appearance of " + n)
        else
            (inp, ())

let (>>=) a f = parsor.Bind(a, f)
let (>>.) a b = parsor{ let! av = a in return! b }
let (.>>) a b = parsor{ let! av = a in let! bv = b in return av }
let (|>>) a f = parsor{ let! av = a in return f av }
let (<?>) (a : Parsor<'Input, 'Output>) n : Parsor<'Input, 'Output> =
    fun (env, inp) ->
        try
            a(env, inp)
        with
        | FatalError _ ->
            raise <| FatalError(inp.Position, "Expected : " + n)
let (<??>) (a : Parsor<'Input, 'Output>) n : Parsor<'Input, 'Output> =
    fun (env, inp) ->
        try
            a(env, inp)
        with
        | FatalError(pos, msg) ->
            raise <| FatalError(inp.Position, "Expected : " + n + "\nError rised by error at " + pos + " : " + msg)
let (<|>) (a : Parsor<'Input, 'Output>) (b : Parsor<'Input, 'Output>) : Parsor<'Input, 'Output> =
    fun arg ->
        try
            a arg
        with
        | FatalError(pos1, msg1) ->
            try
                b arg
            with
            | FatalError(pos2, msg2) ->
                raise <| FatalError((snd arg).Position, "Error rised by error at " + pos2 + " : " + msg1 + "\nOr rised by error at " + pos2 + " : " + msg2)

let (<!>) a f = condition a f
let (^|) a (b : Parsor<'Input, 'Output>) : Parsor<'Input, 'Output> =
    fun arg ->
        match a arg with
        | None -> b arg
        | Some v -> v
