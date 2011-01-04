module Parsor.Expresion

open Parsor.Core
open Parsor.Combinators

type Assoc =
    | LeftAssoc
    | RightAssoc

// przerobic (zwroc uwage na ok i consumed!!!)
// uwaga na to, że jesli sprawdzamy, czy ok i czy consumed to arg musi byc czysty!!!
type ExpresionParsor<'Input, 'State, 'Output>(atom0 : Parsor<'Input, 'State, 'Output>, name) =
    let mutable atom = atom0
    let mutable infixOperators  = []
    let mutable prefixOperators = []
    let mutable suffixOperators = []

    let rec suffixParsor value prior suff (arg : ParsorArg<'Input, 'State>) =
        match suff with
        | [] ->
            arg.Return value
        | (op, opPrior)::xs when opPrior < prior ->
            suffixParsor value prior xs arg
        | (op, opPrior)::xs ->
            match fst op arg.FreshArg with
            | ParseOk(stat', msg', opFun) ->
                suffixParsor (opFun value) prior suffixOperators (ParsorArg(arg.Input, stat', msg', arg.Ok, arg.Consumed))
            | ParseConsumedOk(inp', stat', msg', opFun) ->
                suffixParsor (opFun value) prior  suffixOperators (ParsorArg(inp', stat', msg', arg.Ok, true))
            | ParseError _
            | ParseFatalError _ ->
                suffixParsor value prior xs arg
            | ParseConsumedError(inp', stat', msg', opFun) ->
                suffixParsor (opFun value) prior suffixOperators (ParsorArg(inp', stat', msg', false, true))
            | ParseConsumedFatalError msg' -> 
                ParseConsumedFatalError msg'
    let rec prefixParsor prior pref (arg : ParsorArg<'Input, 'State>) =
        match pref with
        | [] ->
            match fst atom arg.FreshArg with
            | ParseOk(stat', msg', x) ->
                suffixParsor x prior suffixOperators (ParsorArg(arg.Input, stat', msg', arg.Ok, arg.Consumed))
            | ParseConsumedOk(inp', stat', msg', x) ->
                suffixParsor x prior suffixOperators (ParsorArg(inp', stat', msg', arg.Ok, true))
            | ParseError(stat', msg', x) ->
                suffixParsor x prior suffixOperators (ParsorArg(arg.Input, stat', msg', false, arg.Consumed))
            | ParseConsumedError(inp', stat', msg', x) ->
                suffixParsor x prior suffixOperators (ParsorArg(arg.Input, stat', msg', false, true))
            | ParseFatalError msg' -> 
                arg.ReturnFatalErrorM msg'
            | ParseConsumedFatalError msg' -> 
                ParseConsumedFatalError msg'
        | (op, opPrior)::xs ->
            parsor{
                let! opFunOpt = tryParse op;
                match opFunOpt with
                | None ->
                    return! (prefixParsor prior xs, name)
                | Some opFun ->
                    let! exp = exprParsor opPrior;
                    return! (suffixParsor (opFun exp) prior suffixOperators, name)
            } |> fst <| arg
    and exprParsor prior =
        parsor{
            let! value = (prefixParsor prior prefixOperators, name);
            return! (infixParsor value prior infixOperators, name)
        }
    and infixParsor value prior infx (arg : ParsorArg<'Input, 'State>) =
        match infx with
        | [] ->
            suffixParsor value prior suffixOperators arg
        | (op, assoc, opPrior)::xs when opPrior < prior ->
            infixParsor value prior xs arg
        | (op, assoc, opPrior)::xs ->
            parsor{
                let! opFunOpt = tryParse op;
                match opFunOpt with
                | None ->
                    return! (infixParsor value prior xs, name)
                | Some opFun ->
                    let sndPrior =
                        match assoc with
                        | LeftAssoc -> opPrior + 1
                        | RightAssoc -> opPrior
                    let! arg2 = exprParsor sndPrior
                    let! val2 = (suffixParsor (opFun value arg2) prior suffixOperators, name)
                    return! (infixParsor val2 prior infixOperators, name)
            } |> fst <| arg

    new () = ExpresionParsor(Parsor.Primitives.fatalError "no atom defined", lazy "expresion")
    new (name) = ExpresionParsor(Parsor.Primitives.fatalError "no atom defined", name)
    new (atom0) = ExpresionParsor(atom0, lazy "expresion")

    member this.AddInfixOperator op assoc prior =
        infixOperators <- List.append infixOperators [op, assoc, prior]
    member this.AddPrefixOperator op prior =
        prefixOperators <- List.append prefixOperators [op, prior]
    member this.AddSuffixOperator (op : Parsor<'Input, 'State, 'Output -> 'Output>) prior =
        suffixOperators <- List.append suffixOperators [op, prior]

    member this.Parsor : Parsor<'Input, 'State, 'Output> =
        exprParsor 0

    member this.Atom
        with get() = atom
        and set value = atom <- value