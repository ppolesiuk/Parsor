namespace Parsor.Input

open Parsor.Core

type StringInput(inputString : string, position, line, column) =
    new(inputString) = StringInput(inputString, 0, 1, 1)
    interface IParsorInput<char> with
        member this.IsEmpty =
            position >= inputString.Length
        member this.Head =
            inputString.[position]
        member this.Tail = 
            if inputString.[position] = '\n' then
                StringInput(inputString, position + 1, line + 1, 1) :> IParsorInput<char>
            else
                StringInput(inputString, position + 1, line, column + 1) :> IParsorInput<char>
        member this.Position =
            "line " + line.ToString() + ", column " + column.ToString()

type LazyInput<'T>(position, data : ('T * (unit -> IParsorInput<'T>)) option) as self =
    let mutable tail = self :> IParsorInput<'T>
    let mutable evaluated = false
    interface IParsorInput<'T> with
        member this.IsEmpty =
            data.IsNone
        member this.Head =
            fst data.Value
        member this.Tail =
            if not evaluated then
                tail <- (snd data.Value)();
                evaluated <- true
            tail
        member this.Position = position