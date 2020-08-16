namespace R7rsSmall

open System
open System.Text

type Lexeme =
  | Identifier of string
  | Boolean of bool
  | InexactNumber of float
  | ExactNumber of decimal * decimal
  | Character of char
  | String of string
  | OpeningParenthesis
  | NumberSignOpeningParenthesis
  | NumberSignU8OpeningParenthesis
  | FullStop
  | ClosingParenthesis
  | SingleQuote
  | Backtick
  | Comma
  | CommaAtSign
(*| FoldCaseDirective
  | NoFoldCaseDirective*)

module Lexer =

  let isBinary char = char = '0' || char = '1'

  let isOctal char = char >= '0' || char <= '7'

  let isDecimal char = Char.IsDigit(char)

  let isHexadecimal char =
    isDecimal char || char >= 'a' && char <= 'f'

  let isDigit arity =
    match arity with
    | 2 -> isBinary
    | 8 -> isOctal
    | 10 -> isDecimal
    | 16 -> isHexadecimal
    | _ -> fun _ -> false

  let isAlpha char =
    char
    >= 'a'
    && char <= 'z'
    || char >= 'A' && char <= 'Z'

  let isIdentifierInitial char =
    isAlpha char
    || char = '!'
    || char = '$'
    || char = '%'
    || char = '*'
    || char = '/'
    || char = ':'
    || char = '<'
    || char = '='
    || char = '>'
    || char = '?'
    || char = '^'
    || char = '_'
    || char = '~'

  let isExplicitSign char = char = '+' || char = '-'

  let isIdentifierSubsequent char =
    isIdentifierInitial char
    || isDecimal char
    || char = '.'
    || char = '@'
    || isExplicitSign char

  let isSignSubsequent char =
    isIdentifierInitial char
    || isExplicitSign char
    || char = '@'

  let isDotSubsequent char = isSignSubsequent char || char = '.'

  let isAllowedInString char = char <> '\\' && char <> '"'

  let (|Empty|_|) (text: char ReadOnlyMemory) = if text.IsEmpty then Some() else None

  let (|StartsWith|_|) predicate (text: char ReadOnlyMemory) =
    if not (text.IsEmpty) && predicate text.Span.[0]
    then Some text.Span.[0]
    else None

  let (|StartsWithLiteral|_|) (literal: string) (text: char ReadOnlyMemory) =
    if MemoryExtensions.StartsWith(text.Span, literal.AsSpan())
    then Some()
    else None

  let lookAheadWhile predicate length (text: char ReadOnlyMemory) =
    let rec loop window =
      if window
         >= length
         || not (predicate text.Span.[window]) then
        if window >= 0 then text.Slice(0, window) else ReadOnlyMemory.Empty
      else
        loop (window + 1)

    loop 0

  let consumeLine (text: char ReadOnlyMemory) =
    match text.Span.IndexOfAny('\r', '\n') with
    | -1 -> ReadOnlyMemory.Empty
    | position ->
        if text.Span.[position] = '\r'
           && text.Length > position + 1
           && text.Span.[position + 1] = '\n' then
          text.Slice(position + 2)
        else
          text.Slice(position + 1)

  let hexToChar (hexString: char ReadOnlyMemory) =
    let length = hexString.Length
    let bytes = Array.zeroCreate<byte> (length / 2)

    let rec loop index =
      if index >= length then
        ValueSome (Encoding.UTF8.GetChars bytes).[0]
      else
        match Byte.TryParse(hexString.Span.Slice(index, 2)) with
        | true, byte ->
            bytes.[index / 2] <- byte
            loop (index + 2)
        | false, _ -> ValueNone

    loop 0

  let consumeCharacter text length lexemes =
    match text with
    | StartsWithLiteral "alarm" -> Ok(text.Slice(5), Character '\a' :: lexemes)
    | StartsWithLiteral "backspace" -> Ok(text.Slice(9), Character '\b' :: lexemes)
    | StartsWithLiteral "delete" -> Ok(text.Slice(6), Character '\u007F' :: lexemes)
    | StartsWithLiteral "escape" -> Ok(text.Slice(6), Character '\u001B' :: lexemes)
    | StartsWithLiteral "newline" -> Ok(text.Slice(7), Character '\n' :: lexemes)
    | StartsWithLiteral "null" -> Ok(text.Slice(4), Character '\u0000' :: lexemes)
    | StartsWithLiteral "return" -> Ok(text.Slice(6), Character '\r' :: lexemes)
    | StartsWithLiteral "space" -> Ok(text.Slice(5), Character ' ' :: lexemes)
    | StartsWithLiteral "tab" -> Ok(text.Slice(3), Character '\t' :: lexemes)
    | StartsWithLiteral "x" ->
        let hex =
          lookAheadWhile isHexadecimal length (text.Slice(1))

        if hex.Length <= 0 then
          Error(text, lexemes)
        else
          match hexToChar hex with
          | ValueNone -> Error(text, lexemes)
          | ValueSome char -> Ok(text.Slice(1 + hex.Length), Character char :: lexemes)
    | _ ->
        if length > 0
        then Ok(text.Slice(1), Character text.Span.[0] :: lexemes)
        else Error(text, lexemes)

  let consumeString text length lexemes =
    let rec loop text' characters =
      match text' with
      | StartsWithLiteral "\\\"" -> loop (text'.Slice(2)) ('"' :: characters)
      | StartsWithLiteral "\\\\" -> loop (text'.Slice(2)) ('\\' :: characters)
      | StartsWithLiteral "\\a" -> loop (text'.Slice(2)) ('\a' :: characters)
      | StartsWithLiteral "\\b" -> loop (text'.Slice(2)) ('\b' :: characters)
      | StartsWithLiteral "\\t" -> loop (text'.Slice(2)) ('\t' :: characters)
      | StartsWithLiteral "\\n" -> loop (text'.Slice(2)) ('\n' :: characters)
      | StartsWithLiteral "\\r" -> loop (text'.Slice(2)) ('\r' :: characters)
      | StartsWithLiteral "\\x" ->
          let hex =
            lookAheadWhile isHexadecimal length text'

          if hex.Length <= 0 then
            Error(text, lexemes)
          else
            match hexToChar hex with
            | ValueNone -> Error(text, lexemes)
            | ValueSome character ->
                let text'' = text'.Slice(2 + hex.Length)
                match text'' with
                | StartsWithLiteral ";" -> loop (text''.Slice(1)) (character :: characters)
                | _ -> Error(text, lexemes)
      | StartsWithLiteral "\"" ->
          Ok
            (text'.Slice(1),
             String(List.rev characters |> String.Concat)
             :: lexemes)
      | StartsWith isAllowedInString character -> loop (text'.Slice(1)) (character :: characters)
      | _ -> Error(text, lexemes)

    loop text []

  let consumePrefix text lexemes =
    match text with
    | StartsWithLiteral "#i"
    | StartsWithLiteral "#I" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#b"
        | StartsWithLiteral "#B" -> Ok(text'.Slice(2), false, 2, lexemes)
        | StartsWithLiteral "#o"
        | StartsWithLiteral "#O" -> Ok(text'.Slice(2), false, 8, lexemes)
        | StartsWithLiteral "#d"
        | StartsWithLiteral "#D" -> Ok(text'.Slice(2), false, 10, lexemes)
        | StartsWithLiteral "#x"
        | StartsWithLiteral "#X" -> Ok(text'.Slice(2), false, 16, lexemes)
        | _ -> Ok(text', false, 10, lexemes)
    | StartsWithLiteral "#e"
    | StartsWithLiteral "#E" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#b"
        | StartsWithLiteral "#B" -> Ok(text'.Slice(2), true, 2, lexemes)
        | StartsWithLiteral "#o"
        | StartsWithLiteral "#O" -> Ok(text'.Slice(2), true, 8, lexemes)
        | StartsWithLiteral "#d"
        | StartsWithLiteral "#D" -> Ok(text'.Slice(2), true, 10, lexemes)
        | StartsWithLiteral "#x"
        | StartsWithLiteral "#X" -> Ok(text'.Slice(2), true, 16, lexemes)
        | _ -> Ok(text', true, 10, lexemes)
    | StartsWithLiteral "#b"
    | StartsWithLiteral "#B" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#i"
        | StartsWithLiteral "#I" -> Ok(text'.Slice(2), false, 2, lexemes)
        | StartsWithLiteral "#e"
        | StartsWithLiteral "#E" -> Ok(text'.Slice(2), true, 2, lexemes)
        | _ -> Ok(text', false, 2, lexemes)
    | StartsWithLiteral "#o"
    | StartsWithLiteral "#O" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#i"
        | StartsWithLiteral "#I" -> Ok(text'.Slice(2), false, 8, lexemes)
        | StartsWithLiteral "#e"
        | StartsWithLiteral "#E" -> Ok(text'.Slice(2), true, 8, lexemes)
        | _ -> Ok(text', false, 8, lexemes)
    | StartsWithLiteral "#d"
    | StartsWithLiteral "#D" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#i"
        | StartsWithLiteral "#I" -> Ok(text'.Slice(2), false, 10, lexemes)
        | StartsWithLiteral "#e"
        | StartsWithLiteral "#E" -> Ok(text'.Slice(2), true, 10, lexemes)
        | _ -> Ok(text', false, 10, lexemes)
    | StartsWithLiteral "#x"
    | StartsWithLiteral "#X" ->
        let text' = text.Slice(2)
        match text' with
        | StartsWithLiteral "#i"
        | StartsWithLiteral "#I" -> Ok(text'.Slice(2), false, 16, lexemes)
        | StartsWithLiteral "#e"
        | StartsWithLiteral "#E" -> Ok(text'.Slice(2), true, 16, lexemes)
        | _ -> Ok(text', false, 16, lexemes)
    | _ -> Error(text, lexemes)

  let createNumber exact (arity: int) negate (numerator: char ReadOnlyMemory) (denominator: char ReadOnlyMemory) =
    if exact then
      ExactNumber
        ((if negate then (~-) else id) (decimal (Convert.ToInt64(string numerator, arity))),
         (decimal (Convert.ToInt64(string denominator, arity))))
    else
      InexactNumber
        ((if negate then (~-) else id)
          (double (Convert.ToInt64(numerator.ToString(), arity))
           / double (Convert.ToInt64(denominator.ToString(), arity))))

  (*let consumeExponent text =
    match text with
    |

  let consumeDecimalNumber text *)

  let consumeSign text =
    match text with
    | StartsWithLiteral "+" -> struct (text.Slice(1), false)
    | StartsWithLiteral "-" -> struct (text.Slice(1), true)
    | _ -> struct (text, false)

  let consumeNumber' text exact arity lexemes =
    let struct (text', negate) = consumeSign text

    let numerator =
      lookAheadWhile (isDigit arity) text'.Length text'

    let text'' = text'.Slice numerator.Length
    match text'' with
    | StartsWithLiteral "/" ->
        let denominator =
          lookAheadWhile (isDigit arity) (text''.Length - 1) (text''.Slice(1))

        Ok
          (text''.Slice(1 + denominator.Length),
           if denominator.Length = 0
           then createNumber exact arity negate numerator ("1".AsMemory())
           else createNumber exact arity negate numerator denominator
           :: lexemes)
    | _ ->
        Ok
          (text'',
           createNumber exact arity negate numerator ("1".AsMemory())
           :: lexemes)

  let consumeNumber text lexemes =
    consumePrefix text lexemes
    |> Result.bind (fun (text, exact, arity, lexemes) -> consumeNumber' text exact arity lexemes)

  let lex text =

    let rec loop text' lexemes =
      match text' with
      | Empty -> Ok lexemes
      | StartsWithLiteral " "
      | StartsWithLiteral "\t"
      | StartsWithLiteral "\r"
      | StartsWithLiteral "\n" -> loop (text'.TrimStart(" \t\r\n".AsSpan())) lexemes
      | StartsWithLiteral ";" -> loop (consumeLine text') lexemes
      | StartsWithLiteral "#true" -> loop (text'.Slice(5)) (Boolean true :: lexemes)
      | StartsWithLiteral "#t" -> loop (text'.Slice(2)) (Boolean true :: lexemes)
      | StartsWithLiteral "#false" -> loop (text'.Slice(6)) (Boolean false :: lexemes)
      | StartsWithLiteral "#f" -> loop (text'.Slice(2)) (Boolean false :: lexemes)
      | StartsWithLiteral "#\\" ->
          let text'' = text'.Slice(2)
          consumeCharacter text'' text''.Length lexemes
          |> Result.bind ((<||) loop)
      | StartsWithLiteral "\"" ->
          consumeString (text'.Slice(1)) (text'.Length - 1) lexemes
          |> Result.bind ((<||) loop)
      | StartsWith isIdentifierInitial initial ->
          let subsequent =
            lookAheadWhile isIdentifierSubsequent (text'.Length - 1) (text'.Slice(1))

          loop
            (text'.Slice(1 + subsequent.Length))
            (Identifier(String.Concat(initial, subsequent))
             :: lexemes)
      | StartsWith isExplicitSign explicitSign ->
          let text'' = text'.Slice(1)
          match text'' with
          | StartsWith isSignSubsequent signSubsequent ->
              let subsequent =
                lookAheadWhile isIdentifierSubsequent (text''.Length - 1) (text''.Slice(1))

              loop
                (text''.Slice(1 + subsequent.Length))
                (Identifier(String.Concat(explicitSign, signSubsequent, subsequent))
                 :: lexemes)
          | StartsWithLiteral "." ->
              let text''' = text''.Slice(1)
              match text''' with
              | StartsWith isDotSubsequent dotSubsequent ->
                  let subsequent =
                    lookAheadWhile isIdentifierSubsequent (text'''.Length - 1) (text'''.Slice(1))

                  loop
                    (text'''.Slice(1 + subsequent.Length))
                    (Identifier(String.Concat(explicitSign, dotSubsequent, subsequent))
                     :: lexemes)
              | _ -> Error(text', lexemes)
              | _ -> loop text'' (Identifier(string explicitSign) :: lexemes)
          | StartsWith isDecimal _ ->
              (consumeNumber' text' false 10 lexemes)
              |> Result.bind ((<||) loop)
          | _ -> loop text'' (Identifier(string explicitSign) :: lexemes)
      | StartsWithLiteral "." ->
          let text'' = text'.Slice(1)
          match text'' with
          | StartsWith isDotSubsequent dotSubsequent ->
              let subsequent =
                lookAheadWhile isIdentifierSubsequent (text''.Length - 1) (text''.Slice(1))

              loop
                (text''.Slice(1 + subsequent.Length))
                (Identifier(String.Concat('.', dotSubsequent, subsequent))
                 :: lexemes)
          | StartsWith isDecimal _ -> Error(text, lexemes)
          | _ -> loop (text'.Slice(1)) (FullStop :: lexemes)
      | StartsWithLiteral "(" -> loop (text'.Slice(1)) (OpeningParenthesis :: lexemes)
      | StartsWithLiteral "#(" -> loop (text'.Slice(2)) (NumberSignOpeningParenthesis :: lexemes)
      | StartsWithLiteral "#u8(" -> loop (text'.Slice(4)) (NumberSignU8OpeningParenthesis :: lexemes)
      | StartsWithLiteral ")" -> loop (text'.Slice(1)) (ClosingParenthesis :: lexemes)
      | StartsWithLiteral "#" ->
          consumeNumber text' lexemes
          |> Result.bind ((<||) loop)
      | StartsWithLiteral "'" -> loop (text'.Slice(1)) (SingleQuote :: lexemes)
      | StartsWithLiteral "`" -> loop (text'.Slice(1)) (Backtick :: lexemes)
      | StartsWithLiteral ",@" -> loop (text'.Slice(2)) (CommaAtSign :: lexemes)
      | StartsWithLiteral "," -> loop (text'.Slice(1)) (Comma :: lexemes)
      | StartsWith isDecimal _ ->
          consumeNumber' text' false 10 lexemes
          |> Result.bind ((<||) loop)
      | _ -> Error(text, lexemes)

    loop text [] |> Result.map List.rev
