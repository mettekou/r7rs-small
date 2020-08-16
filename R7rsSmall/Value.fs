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
  | List of Value list
  | Vector of Value []
