namespace Parsor.Environment

open Parsor.Core
open System
open System.IO

type Message =
    | MsgWarning of string * string
    | MsgError of string * string
    | MsgFatalError of string * string

type ConsoleEnvironment(output : TextWriter) =
    new() = ConsoleEnvironment(Console.Out)
    interface IParsorEnvironment with
        member this.Warning(position, message) =
            output.WriteLine("Warning at {0} : {1}", position, message)
        member this.Error(position, message) =
            output.WriteLine("Error at {0} : {1}", position, message)
        member this.Parse parsor input =
            try
                parsor (this, input) |> snd |> Some
            with
            | FatalError(position, message) ->
                output.WriteLine("Error at {0} : {1}", position, message);
                None

type ListEnvironment() =
    let messages = System.Collections.Generic.List<Message>()
    member this.Messages =
        messages :> Message seq
    interface IParsorEnvironment with
        member this.Warning(position, message) =
            MsgWarning(position, message) |> messages.Add
        member this.Error(position, message) =
            MsgError(position, message) |> messages.Add
        member this.Parse parsor input =
            try
                parsor (this, input) |> snd |> Some
            with
            | FatalError(position, message) ->
                MsgFatalError(position, message) |> messages.Add;
                None

type NullEnvironment() =
    interface IParsorEnvironment with
        member this.Warning(position, message) = ()
        member this.Error(position, message) = ()
        member this.Parse parsor input =
            try
                parsor (this, input) |> snd |> Some
            with
            | FatalError(position, message) ->
                None