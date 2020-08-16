namespace R7rsSmall.Tests

open Expecto
open R7rsSmall

module Parser =

  let parseImproperListTest =
    let lexemes =
      [ OpeningParenthesis
        Identifier "x"
        Identifier "y"
        Identifier "."
        Identifier "z"
        ClosingParenthesis ]

    testCase "Parser correctly parses improper list"
    <| fun _ ->
      Expect.equal
        (Parser.parse lexemes)
        (Ok
          (List [ Symbol "x"
                  Symbol "y"
                  Symbol "."
                  Symbol "z" ]))
        ""

  let parseQuotedListTest =
    let lexemes =
      [ SingleQuote
        OpeningParenthesis
        Identifier "x"
        Identifier "y"
        Identifier "z"
        ClosingParenthesis ]

    testCase "Parser correctly parses quoted list"
    <| fun _ ->
      Expect.equal
        (Parser.parse lexemes)
        (Ok
          (Abbreviation
            (Quote,
             List [ Symbol "x"
                    Symbol "y"
                    Symbol "z" ])))
        ""

  let parseNumericalDerivativeDefinitionTest =
    let lexemes =
      [ OpeningParenthesis
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
        ClosingParenthesis ]

    testCase "Parser correctly parses numerical derivative definition"
    <| fun _ ->
      Expect.equal
        (Parser.parse lexemes)
        (Ok
          (List [ Symbol "define"
                  List [ Symbol "diff"
                         Symbol "h"
                         Symbol "f"
                         Symbol "x" ]
                  List [ Symbol "/"
                         List [ Symbol "-"
                                List [ Symbol "f"
                                       List [ Symbol "+"
                                              Symbol "x"
                                              Symbol "h" ] ]
                                List [ Symbol "f"; Symbol "x" ] ]
                         Symbol "h" ] ]))
        ""

  [<Tests>]
  let tests =
    testList
      "Parser tests"
      [ parseImproperListTest
        parseQuotedListTest
        parseNumericalDerivativeDefinitionTest ]
