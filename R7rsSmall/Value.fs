namespace R7rsSmall

type Value =
  | Void
  | Boolean of bool
  | InexactNumber of float
  | ExactNumber of decimal * decimal
  | Character of char
  | String of string
  | Symbol of string
  | StandardProcedure of (Value list -> Result<Value, unit>)
  | Procedure of Map<string, Value> * string list * Expression
  | Pair of Value * Value
  | Nil
  | Vector of Value []

module Value =

  let rec (|List|_|) value =
    match value with
    | Pair (car, cdr) -> Option.map (fun cdr' -> car :: cdr') ((|List|_|) cdr)
    | Nil -> Some []
    | _ -> None

  let list values =
    List.foldBack (fun expression pair -> Pair(expression, pair)) values Nil
