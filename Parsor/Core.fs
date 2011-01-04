module Parsor.Core

type IPosition =
    interface
    end

type PosString(str) =
    interface IPosition
    override this.ToString() = str

type IParsorInput<'Input> =
    interface
        abstract IsEmpty : bool
        abstract Head : 'Input
        abstract Tail : IParsorInput<'Input>
        abstract Position : IPosition
    end

type EmptyParsorInput<'Input>(position) =
    interface IParsorInput<'Input> with
        member this.IsEmpty = true
        member this.Head = System.ArgumentException "Parsor input is empty." |> raise
        member this.Tail = System.ArgumentException "Parsor input is empty." |> raise
        member this.Position = position

type LazyParsorInput<'Input>(head,tailFunc,position) =
    let tail = lazy tailFunc()
    interface IParsorInput<'Input> with
        member this.IsEmpty = false
        member this.Head = head
        member this.Tail = tail.Value            
        member this.Position = position

type EagerParsorInput<'Input>(head,tail,position) =
    interface IParsorInput<'Input> with
        member this.IsEmpty = false
        member this.Head = head
        member this.Tail = tail            
        member this.Position = position

type ParsorInputBuilderLazy() =
    member this.Zero() = 
        EmptyParsorInput<'a>(PosString "end of input") :> IParsorInput<'a>
    member this.Combine(a : IParsorInput<'a>, b : IParsorInput<'a>) : IParsorInput<'a> =
        if a.IsEmpty then b
        else LazyParsorInput<'a>(a.Head, (fun () -> this.Combine(a.Tail, b)), a.Position) :> IParsorInput<'a>
    member this.Bind(a : IParsorInput<'a>, f : 'a -> IParsorInput<'b>) : IParsorInput<'b> =
        if a.IsEmpty then EmptyParsorInput<'b>(a.Position) :> IParsorInput<'b>
        else this.Combine(f (a.Head), this.Bind(a.Tail, f))
    member this.Delay(f : unit -> IParsorInput<'a>) = 
        f()    
    member this.For(x : seq<'a>, f : 'a -> IParsorInput<'b>) : IParsorInput<'b> =
        let rec lazyCombine (a : IParsorInput<'c>) b =
            if a.IsEmpty then b()
            else LazyParsorInput<'c>(a.Head, (fun () -> lazyCombine a.Tail b), a.Position) :> IParsorInput<'c>
        in
            if Seq.isEmpty x then this.Zero()
            else lazyCombine (x |> Seq.head |> f) (fun () -> this.For(Seq.skip 1 x, f))
    member this.Return x =
        EagerParsorInput(x, EmptyParsorInput(PosString "end of input"), PosString "somewhere") :> IParsorInput<'a>
    member this.RefurnFrom x = x
    member this.Yield x =
        EagerParsorInput(x, EmptyParsorInput(PosString "end of input"), PosString "somewhere") :> IParsorInput<'a>
    member this.YiledFrom x = x    

type ParsorInputBuilderEager() =
    member this.Zero() = 
        EmptyParsorInput<'a>(PosString "end of input") :> IParsorInput<'a>
    member this.Combine(a : IParsorInput<'a>, b : IParsorInput<'a>) : IParsorInput<'a> =
        if a.IsEmpty then b
        else EagerParsorInput<'a>(a.Head, this.Combine(a.Tail, b), a.Position) :> IParsorInput<'a>
    member this.Bind(a : IParsorInput<'a>, f : 'a -> IParsorInput<'b>) : IParsorInput<'b> =
        if a.IsEmpty then EmptyParsorInput<'b>(a.Position) :> IParsorInput<'b>
        else this.Combine(f (a.Head), this.Bind(a.Tail, f))
    member this.Delay(f : unit -> IParsorInput<'a>) = 
        f()    
    member this.For(x : seq<'a>, f : 'a -> IParsorInput<'b>) : IParsorInput<'b> =
        Seq.fold (fun s a -> this.Combine(s, f a)) (this.Zero()) x
    member this.Return x =
        EagerParsorInput(x, EmptyParsorInput(PosString "end of input"), PosString "somewhere") :> IParsorInput<'a>
    member this.RefurnFrom x = x
    member this.Yield x =
        EagerParsorInput(x, EmptyParsorInput(PosString "end of input"), PosString "somewhere") :> IParsorInput<'a>
    member this.YiledFrom x = x    

type ParsorMessage =
    | MsgMessage of Lazy<string>
    | MsgError of IPosition * Lazy<string>
    | MsgWarning of IPosition * Lazy<string>
    | MsgFatalError of IPosition * Lazy<string>

type ParseResult<'Input, 'State, 'Output> = 
    | ParseConsumedOk of IParsorInput<'Input> * 'State * ParsorMessage list * 'Output
    | ParseOk of 'State * ParsorMessage list * 'Output
    | ParseConsumedError of IParsorInput<'Input> * 'State * ParsorMessage list * 'Output
    | ParseError of 'State * ParsorMessage list * 'Output
    | ParseConsumedFatalError of ParsorMessage list
    | ParseFatalError of ParsorMessage list

type ParsorArg<'Input, 'State> (input, state, messages, ok, consumed) =
    member this.Input    : IParsorInput<'Input> = input
    member this.State    : 'State               = state
    member this.Messages : ParsorMessage list   = messages
    member this.Ok       : bool                 = ok
    member this.Consumed : bool                 = consumed

    member this.FreshArg =
        ParsorArg(input, state, messages, true, false)

    member this.Return v =
        if consumed then
            (if ok then ParseConsumedOk else ParseConsumedError) (input, state, messages, v)
        else
            (if ok then ParseOk else ParseError) (state, messages, v)
    member this.ReturnS stat v =
        if consumed then
            (if ok then ParseConsumedOk else ParseConsumedError) (input, stat, messages, v)
        else
            (if ok then ParseOk else ParseError) (stat, messages, v)
    member this.ReturnM msg v =
        if consumed then
            (if ok then ParseConsumedOk else ParseConsumedError) (input, state, msg, v)
        else
            (if ok then ParseOk else ParseError) (state, msg, v)
    member this.ReturnSM stat msg v =
        if consumed then
            (if ok then ParseConsumedOk else ParseConsumedError) (input, stat, msg, v)
        else
            (if ok then ParseOk else ParseError) (stat, msg, v)
    member this.ReturnISM inp stat msg v =
        if consumed then
            (if ok then ParseConsumedOk else ParseConsumedError) (inp, stat, msg, v)
        else
            (if ok then ParseOk else ParseError) (stat, msg, v)
    member this.ReturnConsumedI inp v =
        (if ok then ParseConsumedOk else ParseConsumedError)(inp, state, messages, v)            
    member this.ReturnConsumedIS inp stat v =
        (if ok then ParseConsumedOk else ParseConsumedError)(inp, stat, messages, v)            
    member this.ReturnConsumedISM inp stat msg v =
        (if ok then ParseConsumedOk else ParseConsumedError)(inp, stat, msg, v)            
    member this.ReturnError v =
        if consumed then 
            ParseConsumedError(input, state, messages, v)
        else
            ParseError(state, messages, v)
    member this.ReturnErrorM msg v =
        if consumed then 
            ParseConsumedError(input, state, msg, v)
        else
            ParseError(state, msg, v)
    member this.ReturnErrorSM stat msg v =
        if consumed then 
            ParseConsumedError(input, stat, msg, v)
        else
            ParseError(stat, msg, v)
    member this.ReturnErrorE err v =
        if consumed then 
            ParseConsumedError(input, state, MsgError(input.Position, err)::messages, v)
        else
            ParseError(state, MsgError(input.Position, err)::messages, v)
    member this.ReturnErrorEC err msgC v =
        if consumed then 
            ParseConsumedError(input, state, MsgError(input.Position, err)::msgC, v)
        else
            ParseError(state, MsgError(input.Position, err)::msgC, v)
    member this.ReturnErrorSE stat err v =
        if consumed then 
            ParseConsumedError(input, stat, MsgError(input.Position, err)::messages, v)
        else
            ParseError(stat, MsgError(input.Position, err)::messages, v)
    member this.ReturnErrorSEC stat err msgC v =
        if consumed then 
            ParseConsumedError(input, stat, MsgError(input.Position, err)::msgC, v)
        else
            ParseError(stat, MsgError(input.Position, err)::msgC, v)
    member this.ReturnErrorFE err v =
        if consumed then 
            ParseConsumedError(input, state, MsgFatalError(input.Position, err)::messages, v)
        else
            ParseError(state, MsgError(input.Position, err)::messages, v)
    member this.ReturnFatalErrorM msg =
        if consumed then
            ParseConsumedFatalError msg
        else
            ParseFatalError msg
    member this.ReturnFatalErrorE err =
        if consumed then
            ParseConsumedFatalError (MsgError(input.Position, err)::messages)
        else
            ParseFatalError (MsgError(input.Position, err)::messages)
    member this.ReturnFatalErrorEC err msgC =
        if consumed then
            ParseConsumedFatalError (MsgError(input.Position, err)::msgC)
        else
            ParseFatalError (MsgError(input.Position, err)::msgC)
    member this.ReturnFatalErrorFE err =
        if consumed then
            ParseConsumedFatalError (MsgFatalError(input.Position, err)::messages)
        else
            ParseFatalError (MsgFatalError(input.Position, err)::messages)

type Parsor<'Input, 'State, 'Output> = 
    (ParsorArg<'Input, 'State> -> ParseResult<'Input, 'State, 'Output>) * Lazy<string>

type ParsorBuilder() =
    member this.Bind(p : Parsor<'i, 's, 'a>, f : 'a -> Parsor<'i, 's, 'b>) : Parsor<'i, 's, 'b> =
        ((
            fun arg ->
                match fst p arg with
                | ParseConsumedOk(inp', stat', msg', vp) -> 
                    fst (f vp) (ParsorArg(inp', stat', msg', arg.Ok, true))
                | ParseOk(stat', msg', vp) -> 
                    fst (f vp) (ParsorArg(arg.Input, stat', msg', arg.Ok, arg.Consumed))
                | ParseConsumedError(inp', stat', msg', vp) -> 
                    fst (f vp) (ParsorArg(inp', stat', msg', false, true))
                | ParseError(stat', msg', vp) ->
                    fst (f vp) (ParsorArg(arg.Input, stat', msg', false, arg.Consumed))
                | ParseFatalError msg' -> ParseFatalError msg'
                | ParseConsumedFatalError msg' -> ParseConsumedFatalError msg'
        ), snd p)
    member this.Return x : Parsor<'i, 's, 'a> =
        ((
            fun arg ->
                arg.Return x
        ), lazy "nothing")
    member this.ReturnFrom x : Parsor<'i, 's, 'a> = x
    member this.For(s : 'a seq, f : 'a -> Parsor<'i, 's, 'b>) : Parsor<'i, 's, unit> =
        if Seq.isEmpty s then this.Return ()
        else this.Bind(Seq.head s |> f, fun _ -> this.For(Seq.skip 1 s, f))
        
let parsorInputLazy = ParsorInputBuilderLazy()
let parsorInput = ParsorInputBuilderEager()
let parsor = ParsorBuilder()

type ParsorResult<'T> =
    | Success of 'T
    | Error

type OutputConsole() = 
    abstract WriteLine       : string -> unit
    abstract WriteMessage    : string -> unit
    abstract WriteError      : IPosition * string -> unit
    abstract WriteWarning    : IPosition * string -> unit
    abstract WriteFatalError : IPosition * string -> unit

    default this.WriteLine msg = 
        System.Console.WriteLine msg
    default this.WriteMessage msg = 
        this.WriteLine msg
    default this.WriteError(pos, msg) =
        this.WriteLine ("Error at " + pos.ToString() + " : " + msg)
    default this.WriteWarning(pos, msg) =
        this.WriteLine ("Warning at " + pos.ToString() + " : " + msg)
    default this.WriteFatalError(pos, msg) =
        this.WriteLine ("Fatal error at " + pos.ToString() + " : " + msg)

    member this.WriteAllMessages msgs =
        for msg in List.rev msgs do
            match msg with
            | MsgMessage m -> this.WriteMessage m.Value
            | MsgError(p, m) -> this.WriteError(p, m.Value)
            | MsgWarning(p, m) -> this.WriteWarning(p, m.Value)
            | MsgFatalError(p, m) -> this.WriteFatalError(p, m.Value)
    
let parse (p : Parsor<'i, 's, 'o>) stat0 (console : OutputConsole) inp : ParsorResult<'o> =
    match fst p (ParsorArg(inp, stat0, [], true, false)) with
    | ParseConsumedOk(_, _, msg, out)
    | ParseOk(_, msg, out) ->
        console.WriteAllMessages msg;
        Success out
    | ParseConsumedError(_, _, msg, _)
    | ParseError(_, msg, _)
    | ParseConsumedFatalError msg
    | ParseFatalError msg ->
        console.WriteAllMessages msg;
        Error