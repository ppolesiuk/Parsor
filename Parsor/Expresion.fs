module Parsor.Expresion

open Parsor.Core
open Parsor.Combinators

type Assoc =
    | LeftAssoc
    | RightAssoc

type ExpresionParsor<'Input, 'Output>(atom0 : Parsor<'Input, 'Output>) =
    let mutable atom = atom0
    let mutable infixOperators  = []
    let mutable prefixOperators = []
    let mutable suffixOperators = []

    let rec suffixParsor value prior suff (env, inp) =
        match suff with
        | [] -> (inp, value)
        | (op, opPrior)::xs when opPrior < prior ->
            suffixParsor value prior xs (env, inp)
        | (op, opPrior)::xs ->
            try
                let (rInp, opFun) = op(env, inp) in
                    suffixParsor (opFun value) prior suffixOperators (env, rInp)
            with
            | FatalError _ ->
                suffixParsor value prior xs (env, inp)
    let rec prefixParsor prior pref (env, inp) =
        match pref with
        | [] ->
            parsor{
                let! x = atom
                return! suffixParsor x prior suffixOperators
            } <| (env, inp)
        | (op, opPrior)::xs -> 
            let (rInp, opFunOpt) = (tryParse op) (env, inp);
            match opFunOpt with
            | None ->
                prefixParsor prior xs (env, inp)
            | Some opFun ->
                parsor{
                    let! exp = exprParsor opPrior
                    return! suffixParsor (opFun exp) prior suffixOperators
                } <| (env, rInp)
    and exprParsor prior =
        parsor{
            let! value = prefixParsor prior prefixOperators
            return! infixParsor value prior infixOperators
        }
    and infixParsor value prior infx (env, inp) =
        match infx with
        | [] ->
            suffixParsor value prior suffixOperators (env, inp)
        | (op, assoc, opPrior)::xs when opPrior < prior ->
            infixParsor value prior xs (env, inp)
        | (op, assoc, opPrior)::xs ->
            let (rInp, opFunOpt) = (tryParse op) (env, inp);
            match opFunOpt with
            | None ->
                infixParsor value prior xs (env, inp)
            | Some opFun ->
                let sndPrior =
                    match assoc with
                    | LeftAssoc -> opPrior + 1
                    | RightAssoc -> opPrior
                parsor{
                    let! arg2 = exprParsor sndPrior
                    let! val2 = suffixParsor (opFun value arg2) prior suffixOperators
                    return! infixParsor val2 prior infixOperators
                } <| (env, rInp)

    new () = ExpresionParsor(Parsor.Primitives.fatalError "no atom defined")

    member this.AddInfixOperator op assoc prior =
        infixOperators <- List.append infixOperators [op, assoc, prior]
    member this.AddPrefixOperator op prior =
        prefixOperators <- List.append prefixOperators [op, prior]
    member this.AddSuffixOperator (op : Parsor<'Input, 'Output -> 'Output>) prior =
        suffixOperators <- List.append suffixOperators [op, prior]

    member this.Parsor : Parsor<'Input, 'Output> =
        exprParsor 0

    member this.Atom
        with get() = atom
        and set value = atom <- value