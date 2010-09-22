module Parsor.Core

exception FatalError of string * string

type IParsorInput<'Input> =
    interface
        abstract IsEmpty : bool
        abstract Head : 'Input
        abstract Tail : IParsorInput<'Input>
        abstract Position : string
    end

type ParsorInputBuilder() =
    class
    end

type IParsorEnvironment =
    interface
        abstract Warning : string * string -> unit
        abstract Error : string * string -> unit
        abstract Parse : Parsor<'Input, 'Output> -> IParsorInput<'Input> -> 'Output option
    end

and Parsor<'Input, 'Output> = IParsorEnvironment * IParsorInput<'Input> -> (IParsorInput<'Input> * 'Output)

type ParsorBuilder() =
    member this.Bind(p : Parsor<'i, 'a>, f : 'a -> Parsor<'i, 'b>) : Parsor<'i, 'b> =
        fun (env, inp) ->
            let (inp2, out) = p(env, inp) in (f out)(env, inp2)
    member this.Return x : Parsor<'i, 'a> =
        fun (env, inp) -> (inp, x)
    member this.ReturnFrom x : Parsor<'i, 'a> = x
    member this.For(s : 'a seq, f : 'a -> Parsor<'i, 'b>) : Parsor<'i, unit> =
        fun (env, inp) ->
            let mutable mInp = inp
            for a in s do
                let (rInp, _) = f a (env, mInp) in mInp <- rInp
            (mInp, ())

let parsorInput = ParsorInputBuilder()
let parsor = ParsorBuilder()

type ParsorResult<'T> =
    | Success of 'T
    | Error of string * string

let parse (p : Parsor<'i, 'o>) env inp : ParsorResult<'o> =
    try
        let (_, r) = p(env, inp) in Success r
    with
    | FatalError(pos, msg) -> Error(pos, msg)
