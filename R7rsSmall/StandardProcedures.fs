namespace R7rsSmall

open System.Runtime.InteropServices

module StandardProcedures =

  let ``eqv?`` values =
    match values with
    | [ Boolean first; Boolean second ] -> Ok(Boolean(first = second))
    | [ ExactNumber (firstNumerator, firstDenominator); ExactNumber (secondNumerator, secondDenominator) ] ->
        Ok
          (Boolean
            (firstNumerator
             / firstDenominator = secondNumerator / secondDenominator))
    | [ _; _ ] -> Ok(Boolean false)
    | _ -> Error()

  let plus' firstNumber secondNumber =
    match firstNumber, secondNumber with
    | ExactNumber (firstNumerator, firstDenominator), ExactNumber (secondNumerator, secondDenominator) ->
        let denominator = firstDenominator * secondDenominator
        Ok
          (ExactNumber
            (secondDenominator
             * firstNumerator
             + firstDenominator * secondNumerator,
             denominator))
    | InexactNumber firstNumber, InexactNumber secondNumber -> Ok(InexactNumber(firstNumber + secondNumber))
    | ExactNumber (firstNumerator, firstDenominator), InexactNumber secondNumber ->
        Ok
          (InexactNumber
            (float (firstNumerator / firstDenominator)
             + secondNumber))
    | InexactNumber firstNumber, ExactNumber (secondNumerator, secondDenominator) ->
        Ok
          (InexactNumber
            (firstNumber
             + float (secondNumerator / secondDenominator)))
    | _, _ -> Error()

  let plus values =
    List.fold (fun soFar next -> Result.bind (fun first -> plus' first next) soFar) (Ok(ExactNumber(0M, 1M))) values

  let list values = Ok(Value.list values)

  let car values =
    match values with
    | [ Value.List (head :: _) ] -> Ok head
    | _ -> Error()

  let cdr values =
    match values with
    | [ Value.List (_ :: tail) ] -> Ok(Value.list tail)
    | _ -> Error()

  let features values =
    match values with
    | [] ->
        let os =
          if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
          then [ Symbol "windows" ]
          else if RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
          then [ Symbol "posix"; Symbol "linux" ]
          else if RuntimeInformation.IsOSPlatform(OSPlatform.FreeBSD)
          then [ Symbol "posix"; Symbol "freebsd" ]
          else if RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
          then [ Symbol "posix"; Symbol "darwin" ]
          else []

        Ok
          (Value.list
            ([ Symbol "r7rs"
               Symbol "ieee-float"
               Symbol "clr" ]
             @ os))
    | _ -> Error()
