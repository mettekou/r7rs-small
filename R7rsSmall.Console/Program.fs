// Learn more about F# at http://fsharp.org

open R7rsSmall
open System

let rec repl environment =
  printf "> "
  match Console.ReadLine().AsMemory() |> Lexer.lex with
  | Error error ->
      printfn "Lexical errror: %A" error
      repl environment
  | Ok lexemes ->
      match Parser.parse lexemes with
      | Error error ->
          printfn "Syntactic error: %A" error
          repl environment
      | Ok (Expression.List [ Symbol "exit" ]) -> 0
      | Ok expression ->
          match Evaluator.evaluate expression environment with
          | Error error ->
              printfn "Evaluation error: %A" error
              repl environment
          | Ok (value, environment') ->
              Printer.print value |> printfn "%s"
              repl environment'

[<EntryPoint>]
let main _ =
  printfn "Welcome to R7RS-small (https://small.r7rs.org/)."
  repl Evaluator.globalEnvironment
