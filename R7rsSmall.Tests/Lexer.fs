namespace R7rsSmall.Tests

open Expecto
open R7rsSmall
open System

module Lexer =

  let consumeCarriageReturnLineFeedTest =
    testCase "Lexer correctly consumes carriage return and line feed"
    <| fun _ ->
      let memory = "Hello, World!\r\nNew line".AsMemory()
      Expect.equal ((Lexer.consumeLine memory).ToString()) "New line" ""

  let consumeCarriageReturnTest =
    testCase "Lexer correctly consumes carriage return"
    <| fun _ ->
      let memory = "Hello, World!\rNew line".AsMemory()
      Expect.equal ((Lexer.consumeLine memory).ToString()) "New line" ""

  let consumeLineFeedTest =
    testCase "Lexer correctly consumes line feed"
    <| fun _ ->
      let memory = "Hello, World!\nNew line".AsMemory()
      Expect.equal ((Lexer.consumeLine memory).ToString()) "New line" ""

  let lexTrueTest =
    testCase "Lexer correctly lexes true constant"
    <| fun _ -> Expect.equal (Lexer.lex ("#t".AsMemory())) (Ok [ Lexeme.Boolean true ]) ""

  let lexInexactDecimalFractionTest =
    testCase "Lexer correctly lexes inexact decimal fraction"
    <| fun _ -> Expect.equal (Lexer.lex ("1/5".AsMemory())) (Ok [ Lexeme.InexactNumber(1. / 5.) ]) ""

  let lexStringVariableDefinitionTest =
    testCase "Lexer correctly lexes string variable definition"
    <| fun _ ->
      Expect.equal
        (Lexer.lex ("(define hello-world \"Hello, World!\")".AsMemory()))
        (Ok [ OpeningParenthesis
              Identifier "define"
              Identifier "hello-world"
              Lexeme.String "Hello, World!"
              ClosingParenthesis ])
        ""

  let lexNumericalDerivativeDefinitionTest =
    testCase "Lexer correctly lexes numerical derivative definition"
    <| fun _ ->
      Expect.equal
        (Lexer.lex ("(define (diff h f x) (/ (- (f (+ x h)) (f x)) h))".AsMemory()))
        (Ok [ OpeningParenthesis
              Identifier "define"
              OpeningParenthesis
              Identifier "diff"
              Identifier "h"
              Identifier "f"
              Identifier "x"
              ClosingParenthesis
              OpeningParenthesis
              Identifier "/"
              OpeningParenthesis
              Identifier "-"
              OpeningParenthesis
              Identifier "f"
              OpeningParenthesis
              Identifier "+"
              Identifier "x"
              Identifier "h"
              ClosingParenthesis
              ClosingParenthesis
              OpeningParenthesis
              Identifier "f"
              Identifier "x"
              ClosingParenthesis
              ClosingParenthesis
              Identifier "h"
              ClosingParenthesis
              ClosingParenthesis ])
        ""

  [<Tests>]
  let tests =
    testList
      "Lexer tests"
      [ consumeCarriageReturnLineFeedTest
        consumeCarriageReturnTest
        consumeLineFeedTest
        lexTrueTest
        lexInexactDecimalFractionTest
        lexStringVariableDefinitionTest
        lexNumericalDerivativeDefinitionTest ]
