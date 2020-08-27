namespace R7rsSmall

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

  let car values =
    match values with
    | [ List (head :: _) ] -> Ok head
    | _ -> Error()

  let cdr values =
    match values with
    | [ List (_ :: tail) ] -> Ok (List tail)
    | _ -> Error()