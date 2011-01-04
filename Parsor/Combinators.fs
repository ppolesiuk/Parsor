module Parsor.Combinators

open Parsor.Core

exception InfiniteLoopException of string

let mkError<'Input, 'State> : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            arg.ReturnError ()
    ), lazy "nothing")

let skip (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, unit> =
    parsor{
        let! x = p;
        return ()
    }

let skipMany(p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, unit> =
    let rec resultParsor (arg : ParsorArg<'Input, 'State>) =
        match fst p (arg.FreshArg) with
        | ParseConsumedOk(inp', state', msg', _) -> 
            resultParsor <| ParsorArg(inp', state', msg', arg.Ok, true)
        | ParseOk(state', msg', _) -> 
            raise <| InfiniteLoopException ("Many of not-consuming parsors at " + arg.Input.Position.ToString())
        | ParseFatalError _
        | ParseError _ -> 
            arg.Return ()
        | ParseConsumedError(inp', state', msg', _) ->
            resultParsor <| ParsorArg(inp', state', msg', false, true)
        | ParseConsumedFatalError msg' ->
            arg.ReturnErrorM msg' ()
    in (resultParsor, lazy("many of " + (snd p).Value))

let skipMany1 p =
    parsor{
        do! skip p
        return! skipMany p
    }

let many (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, 'T list> =
    let name = lazy("many of " + (snd p).Value);
    let rec resultParsor revres (arg : ParsorArg<'Input, 'State>) =
        match fst p (arg.FreshArg) with
        | ParseConsumedOk(inp', state', msg', v) ->
            resultParsor (v::revres) (ParsorArg(inp', state', msg', true, arg.Ok))
        | ParseOk(state', msg', _) -> 
            raise <| InfiniteLoopException ("Many of not-consuming parsors at " + arg.Input.Position.ToString())
        | ParseFatalError _
        | ParseError _ ->
            arg.Return (List.rev revres)
        | ParseConsumedError(inp', state', msg', v) ->
            resultParsor (v::revres) (ParsorArg(inp', state', msg', true, false))
        | ParseConsumedFatalError msg' ->
            arg.ReturnErrorM msg' (List.rev revres)
    in (resultParsor [], name)

let many1 p =
    parsor{
        let! x = p
        let! xs = many p
        return x::xs
    }

let tryParse (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, 'T option> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', v) -> 
                arg.ReturnSM stat' msg' (Some v)
            | ParseConsumedOk(inp', stat', msg', v) ->
                arg.ReturnConsumedISM inp' stat' msg' (Some v)
            | ParseError _
            | ParseFatalError _ ->
                arg.Return None
            | ParseConsumedError(inp', stat', msg', v) -> 
                ParseConsumedError(inp', stat', msg', Some v)
            | ParseConsumedFatalError msg' -> 
                ParseConsumedFatalError msg'
    ), lazy((snd p).Value + " or nothing"))

let altParse alternative (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, 'T> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', v) -> 
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) ->
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError _
            | ParseFatalError _ ->
                arg.Return alternative
            | ParseConsumedError(inp', stat', msg', v) -> 
                ParseConsumedError(inp', stat', msg', v)
            | ParseConsumedFatalError msg' -> 
                ParseConsumedFatalError msg'
    ), lazy((snd p).Value + " or nothing"))

let foldl func stat (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, 'S> =
    let rec resultParsor fstat (arg : ParsorArg<'Input, 'State>) =
        match fst p (arg.FreshArg) with
        | ParseConsumedOk(inp', state', msg', v) ->
            resultParsor (func fstat v) (ParsorArg(inp', state', msg', true, arg.Ok))
        | ParseOk(state', msg', _) -> 
            raise <| InfiniteLoopException ("Many of not-consuming parsors at " + arg.Input.Position.ToString())
        | ParseFatalError _
        | ParseError _ ->
            arg.Return fstat
        | ParseConsumedError(inp', state', msg', v) ->
            resultParsor (func fstat v) (ParsorArg(inp', state', msg', true, false))
        | ParseConsumedFatalError msg' ->
            arg.ReturnErrorM msg' fstat
    in (resultParsor stat, lazy("many of " + (snd p).Value))

let rec foldr func (p : Parsor<'Input, 'State, 'T>) stat : Parsor<'Input, 'State, 'S> =
    parsor{
        let! vopt = tryParse p;
        match vopt with
        | None -> return stat
        | Some v ->
            let! s = foldr func p stat
            return func v s
    }

let multiOption(opt : Parsor<'Input, 'State, 'T> seq) : Parsor<'Input, 'State, 'T> =
    let name = 
        lazy(
            if Seq.isEmpty opt then "nothing" 
            else 
                Seq.fold (fun a (_,b : Lazy<string>) -> a + " or " + b.Value) ((snd (Seq.head opt)).Value) (Seq.skip 1 opt)
        )
    let rec resultParser opt =
        fun (arg : ParsorArg<'Input, 'State>) ->
            if Seq.isEmpty opt then
                arg.ReturnFatalErrorE <| lazy ("Expected : " + name.Value)
            else
            match fst (Seq.head opt) (arg.FreshArg) with
            | ParseOk(stat', msg', v) ->
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) ->
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError _
            | ParseFatalError _ ->
                resultParser (Seq.skip 1 opt) arg
            | r -> r
    in (resultParser opt, name)

let box (p : Parsor<'Input, 'State, 'T>) : Parsor<'Input, 'State, 'T> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', v) ->
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) ->
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError(stat', msg', v)
            | ParseConsumedError(_, stat', msg', v) ->
                arg.ReturnErrorSM stat' msg' v
            | ParseConsumedFatalError msg'
            | ParseFatalError msg' ->
                arg.ReturnFatalErrorM msg'
    ), snd p)

let condition (cond : Parsor<'Input, 'State, 'A>) (p : 'A -> Parsor<'Input, 'State, 'B>) : Parsor<'Input, 'State, 'B> =
    ((
        fun arg ->
            match fst cond (arg.FreshArg) with
            | ParseOk(stat', msg', v) ->
                fst (p v) (ParsorArg(arg.Input, stat', msg', arg.Ok, arg.Consumed))
            | ParseConsumedOk(inp', stat', msg', v) ->
                fst (p v) (ParsorArg(inp', stat', msg', arg.Ok, true))
            | ParseError(_, msg', _)
            | ParseConsumedError(_, _, msg', _)
            | ParseFatalError msg'
            | ParseConsumedFatalError msg' ->
                arg.ReturnFatalErrorM msg'
    ), snd cond)

let lookAhead (p : Parsor<'Input, 'State, 'Output>) : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', v)
            | ParseConsumedOk(_, stat', msg', v) ->
                arg.ReturnSM stat' msg' v
            | ParseError(stat', msg', v)
            | ParseConsumedError(_, stat', msg', v) -> 
                arg.ReturnErrorSM stat' msg' v
            | ParseConsumedFatalError msg'
            | ParseFatalError msg' ->
                ParseFatalError msg'
    ), snd p)

let followedBy (p : Parsor<'Input, 'State, 'Output>) : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', _) 
            | ParseConsumedOk(_, stat', msg', _) -> 
                arg.ReturnSM stat' msg' ()
            | ParseError(stat', msg', _)
            | ParseConsumedError(_, stat', msg', _) -> 
                arg.ReturnErrorSM stat' msg' ()
            | ParseFatalError msg'
            | ParseConsumedFatalError msg' -> 
                arg.ReturnErrorM msg' ()
    ), snd p)

let notFollowedBy (p : Parsor<'Input, 'State, 'Output>) : Parsor<'Input, 'State, unit> =
    ((
        fun arg ->
            match fst p (arg.FreshArg) with
            | ParseOk(stat', msg', _) 
            | ParseConsumedOk(_, stat', msg', _)
            | ParseConsumedError(_, stat', msg', _) -> 
                arg.ReturnErrorSEC stat' (lazy ("Unexpected appearance of " + (snd p).Value)) msg' ()
            | ParseConsumedFatalError msg' ->
                arg.ReturnErrorEC (lazy ("Unexpected appearance of " + (snd p).Value)) msg' ()
            | ParseError _
            | ParseFatalError _ ->
                arg.Return ()
    ), lazy ("not " + (snd p).Value))

let debug (p : Parsor<'Input, 'State, 'Output>) : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            System.Console.WriteLine("DEBUG : Enter {0} : msg[{1}] {2} {3} at {4}\nState = {5}", 
                (snd p).Value,
                arg.Messages.Length,
                (if arg.Ok then "Ok" else "Error"),
                (if arg.Consumed then "Consumed" else "Init"),
                arg.Input.Position,
                arg.State);
            let res = fst p arg;
            match res with
            | ParseOk(stat', msg', v) ->
                System.Console.WriteLine("DEBUG : Leave {0} Ok : msg[{1}]\nState = {2}\nValue = {3}",
                    (snd p).Value,
                    msg'.Length,
                    stat',
                    v);
            | ParseConsumedOk(inp', stat', msg', v) ->
                System.Console.WriteLine("DEBUG : Leave {0} ConsumedOk : msg[{1}] at {2} \nState = {3}\nValue = {4}",
                    (snd p).Value,
                    msg'.Length,
                    inp',
                    stat',
                    v);
            | ParseError(stat', msg', v) ->
                System.Console.WriteLine("DEBUG : Leave {0} Error : msg[{1}]\nState = {2}\nValue = {3}",
                    (snd p).Value,
                    msg'.Length,
                    stat',
                    v);
            | ParseConsumedError(inp', stat', msg', v) ->
                System.Console.WriteLine("DEBUG : Leave {0} ConsumedError : msg[{1}] at {2} \nState = {3}\nValue = {4}",
                    (snd p).Value,
                    msg'.Length,
                    inp',
                    stat',
                    v);
            | ParseFatalError msg' ->
                System.Console.WriteLine("DEBUG : Leave {0} FatalError : msg[{1}]",
                    (snd p).Value,
                    msg'.Length);
            | ParseConsumedFatalError msg' ->
                System.Console.WriteLine("DEBUG : Leave {0} ConsumedFatalError : msg[{1}]",
                    (snd p).Value,
                    msg'.Length);
            res
    ), snd p)

let (>>=) a f = parsor.Bind(a, f)
let (>>.) a b = parsor{ let! av = a in return! b }
let (.>>) a b = parsor{ let! av = a in let! bv = b in return av }
let (|>>) a f = parsor{ let! av = a in return f av }
let (<?>) (a : Parsor<'Input, 'State, 'Output>) n : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            match fst a (arg.FreshArg) with
            | ParseOk(stat', _, v) -> 
                arg.ReturnS stat' v
            | ParseConsumedOk(inp', stat', _, v) ->
                arg.ReturnConsumedIS inp' stat' v
            | ParseError(stat', _, v)
            | ParseConsumedError(_, stat', _, v) ->
                arg.ReturnErrorSE stat' (lazy ("Expected : " + n)) v
            | ParseFatalError _
            | ParseConsumedFatalError _ ->
                arg.ReturnFatalErrorE (lazy ("Expected : " + n))
    ), lazy n)
let (<?!>) (a : Parsor<'Input, 'State, 'Output>) n : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            match fst a (arg.FreshArg) with
            | ParseOk(stat', msg', v) -> 
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) -> 
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError(stat', msg', v)
            | ParseConsumedError(_, stat', msg', v) ->
                arg.ReturnErrorSEC stat' (lazy ("Expected : " + n)) msg' v
            | ParseFatalError msg'
            | ParseConsumedFatalError msg' ->
                arg.ReturnFatalErrorEC (lazy ("Expected : " + n)) msg'
    ), lazy n)
let (<??>) (a : Parsor<'Input, 'State, 'Output>) n : Parsor<'Input, 'State, 'Output> =
    ((
        fun arg ->
            match fst a (arg.FreshArg) with
            | ParseOk(stat', msg', v) -> 
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) -> 
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError(stat', msg', v) ->
                arg.ReturnErrorSEC stat' (lazy ("Expected : " + n)) msg' v
            | ParseFatalError msg' ->
                arg.ReturnFatalErrorEC (lazy ("Expected : " + n)) msg'
            | r -> r
    ), lazy n)
let (<|>) (a : Parsor<'Input, 'State, 'Output>) (b : Parsor<'Input, 'State, 'Output>) : Parsor<'Input, 'State, 'Output> =
    let name = lazy ((snd a).Value + " or " + (snd b).Value) in
    ((
        fun arg ->
            match fst a (arg.FreshArg) with
            | ParseOk(stat', msg', v) ->
                arg.ReturnSM stat' msg' v
            | ParseConsumedOk(inp', stat', msg', v) ->
                arg.ReturnConsumedISM inp' stat' msg' v
            | ParseError(stat', msg', v) ->
                match fst b (arg.FreshArg) with
                | ParseOk(stat'', msg'', v) ->
                    arg.ReturnSM stat'' msg'' v
                | ParseConsumedOk(inp'', stat'', msg'', v) ->
                    arg.ReturnConsumedISM inp'' stat'' msg'' v
                | ParseError _
                | ParseFatalError _ ->
                    arg.ReturnErrorSE stat' (lazy ("Expected : " + name.Value)) v
                | r -> r                    
            | ParseFatalError _ ->
                match fst b (arg.FreshArg) with
                | ParseOk(stat'', msg'', v) ->
                    arg.ReturnSM stat'' msg'' v
                | ParseConsumedOk(inp'', stat'', msg'', v) ->
                    arg.ReturnConsumedISM inp'' stat'' msg'' v
                | ParseError(stat', _, v) ->
                    arg.ReturnErrorSE stat' (lazy ("Expected : " + name.Value)) v
                | ParseFatalError _ ->
                    arg.ReturnFatalErrorE (lazy("Expected : " + name.Value))
                | r -> r
            | r -> r
    ), name)

let (<!>) a f = condition a f
