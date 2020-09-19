namespace R7rsSmall

type Prefix =
  | Quote
  | Quasiquote
  | Unquote
  | UnquoteSplice

type Expression =
  | Boolean of bool
  | InexactNumber of float
  | ExactNumber of decimal * decimal
  | Character of char
  | String of string
  | Symbol of string
  | Pair of Expression * Expression
  | Nil
  | Vector of Expression list
  | ByteVector of Expression list
  | Abbreviation of Prefix * Expression

module Expression =

  let rec (|List|_|) expression =
    match expression with
    | Pair (car, cdr) -> Option.map (fun cdr' -> car :: cdr') ((|List|_|) cdr)
    | Nil -> Some []
    | _ -> None

  let list expressions =
    List.foldBack (fun expression pair -> Pair(expression, pair)) expressions Nil
