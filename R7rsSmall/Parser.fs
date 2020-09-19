namespace R7rsSmall

type ParserState =
  | Ready of Expression
  | InList of Expression list
  | InVector of Expression list
  | InByteVector of Expression list
  | InAbbreviation of Prefix

module Parser =

  let list expressions =
    match expressions with
    | [] -> Nil
    | cdr :: Symbol "." :: car :: expressions' ->
        List.fold (fun soFar next -> Pair(next, soFar)) (Pair(car, cdr)) expressions'
    | _ :: _ -> List.fold (fun soFar next -> Pair(next, soFar)) Nil expressions

  let rec shift stack expression =
    match stack with
    | [] -> Ok [ Ready expression ]
    | Ready _ :: _ -> Error stack
    | InList expressions :: stack' -> Ok(InList(expression :: expressions) :: stack')
    | InVector expressions :: stack' -> Ok(InVector(expression :: expressions) :: stack')
    | InByteVector expressions :: stack' -> Ok(InByteVector(expression :: expressions) :: stack')
    | InAbbreviation prefix :: stack' ->
        match stack' with
        | [] -> Ok([ Ready(Abbreviation(prefix, expression)) ])
        | Ready _ :: _ -> Error(stack)
        | InList expressions :: states'' ->
            Ok
              (InList(Abbreviation(prefix, expression) :: expressions)
               :: states'')
        | InVector expressions :: states'' ->
            Ok
              (InVector(Abbreviation(prefix, expression) :: expressions)
               :: states'')
        | InByteVector expressions :: states'' ->
            Ok
              (InByteVector(Abbreviation(prefix, expression) :: expressions)
               :: states'')
        | InAbbreviation _ :: _ -> shift stack' (Abbreviation(prefix, expression))

  let reduce stack =
    match stack with
    | [] -> Error stack
    | Ready _ :: _ -> Error stack
    | InList expressions :: stack' -> list expressions |> shift stack'
    | InVector expressions :: stack' -> List.rev expressions |> Vector |> shift stack'
    | InByteVector expressions :: stack' -> List.rev expressions |> ByteVector |> shift stack'
    | InAbbreviation _ :: _ -> Error stack

  let shiftAbbreviation prefix stack = InAbbreviation prefix :: stack |> Ok

  let step lexeme stack =
    match lexeme with
    | Identifier name -> shift stack (Symbol name)
    | Lexeme.Boolean value -> shift stack (Boolean value)
    | Lexeme.InexactNumber value -> shift stack (InexactNumber value)
    | Lexeme.ExactNumber (numerator, denominator) -> shift stack (ExactNumber(numerator, denominator))
    | Lexeme.Character value -> shift stack (Character value)
    | Lexeme.String value -> shift stack (String value)
    | OpeningParenthesis -> InList [] :: stack |> Ok
    | NumberSignOpeningParenthesis -> InVector [] :: stack |> Ok
    | NumberSignU8OpeningParenthesis -> InByteVector [] :: stack |> Ok
    | FullStop -> shift stack (Symbol ".")
    | ClosingParenthesis -> reduce stack
    | SingleQuote -> shiftAbbreviation Quote stack
    | Backtick -> shiftAbbreviation Quasiquote stack
    | Comma -> shiftAbbreviation Unquote stack
    | CommaAtSign -> shiftAbbreviation UnquoteSplice stack

  let parse lexemes =
    List.fold (fun stack lexeme -> Result.bind (step lexeme) stack) (Ok []) lexemes
    |> Result.bind (fun states ->
         match states with
         | [ Ready expression ] -> Ok expression
         | _ -> Error(states))
