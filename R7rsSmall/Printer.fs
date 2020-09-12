namespace R7rsSmall

module Printer =

  let rec print value =
    match value with
    | Void -> ""
    | Boolean true -> "#t"
    | Boolean false -> "#f"
    | Character char -> int char |> string
    | InexactNumber value -> string value
    | ExactNumber (numerator, denominator) -> string numerator + "/" + string denominator
    | String string -> "\"" + string + "\""
    | Symbol symbol -> "'" + symbol
    | Procedure _ -> "#<procedure>"
    | List values ->
        "'("
        + (List.map print values |> String.concat " ")
        + ")"
    | Vector values ->
        "#("
        + (Array.map print values |> String.concat " ")
        + ")"
