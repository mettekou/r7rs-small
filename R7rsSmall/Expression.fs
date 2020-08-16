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
  | List of Expression list
  | Vector of Expression list
  | ByteVector of Expression list
  | Abbreviation of Prefix * Expression
