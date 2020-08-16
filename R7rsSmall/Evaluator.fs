namespace R7rsSmall

open System
open System.Text
open System.Text.RegularExpressions

module Evaluator =

  let globalEnvironment =
    Map.ofList [ "eqv?", StandardProcedure(StandardProcedures.``eqv?``)
                 "+", StandardProcedure(StandardProcedures.plus) ]

  let hexToChar hexChars =
    let hexString = String.Concat<char> hexChars
    let length = String.length hexString
    let bytes = Array.zeroCreate<byte> (length / 2)
    for index in 0 .. 2 .. length - 1 do
      bytes.[index / 2] <- Convert.ToByte(hexString.Substring(index, 2), 16)
    (Encoding.UTF8.GetChars bytes).[0]

  let rec chooseOk list =
    match list with
    | [] -> []
    | Ok ok :: tail -> ok :: chooseOk tail
    | _ :: tail -> chooseOk tail

  let isTruish value =
    match value with
    | Boolean false -> false
    | _ -> true

  let isSymbol expression =
    match expression with
    | Expression.Symbol _ -> true
    | _ -> false

  let name expression =
    match expression with
    | Expression.Symbol name ->
        Some(Regex.Replace(name, "\\\\#x[0-9a-f]+;", (fun match' -> hexToChar match'.Value |> string)))
    | _ -> None

  let getParameterNames expressions =
    if List.forall isSymbol expressions then Some(List.choose name expressions) else None

  let rec evaluate (expression: Expression) (environment: Map<string, Value>) =
    match expression with
    | Expression.Boolean value -> Ok(Value.Boolean value, environment)
    | Expression.InexactNumber value -> Ok(InexactNumber value, environment)
    | Expression.ExactNumber (numerator, denominator) -> Ok(ExactNumber(numerator, denominator), environment)
    | Expression.Character value -> Ok(Character value, environment)
    | Expression.String value -> Ok(Value.String value, environment)
    | Expression.Symbol name ->
        match Map.tryFind
                (Regex.Replace(name, "\\\\#x[0-9a-f]+;", (fun match' -> hexToChar match'.Value |> string)))
                environment with
        | None -> Error(expression, environment)
        | Some value -> Ok(value, environment)
    | Expression.List list ->
        match list with
        | [ Expression.Symbol "define"; Expression.Symbol name; expression ] ->
            let result = evaluate expression environment
            match result with
            | Error error -> Error error
            | Ok (value, environment') -> Ok(Value.Void, Map.add name value environment')
        | [ Expression.Symbol "set!"; Expression.Symbol name; expression ] ->
            if Map.containsKey name environment then
              let result = evaluate expression environment
              match result with
              | Error error -> Error error
              | Ok (value, environment') -> Ok(Value.Void, Map.add name value environment')
            else
              Error(expression, environment)
        | [ Expression.Symbol "if"; predicate; consequent; alternative ] ->
            evaluate predicate environment
            |> Result.bind (fun (value, environment') ->
                 if isTruish value then evaluate consequent environment' else evaluate alternative environment')
        | [ Expression.Symbol "lambda"; Expression.List parameterExpressions; body ] ->
            match getParameterNames parameterExpressions with
            | None -> Error(expression, environment)
            | Some parameterNames -> Ok(Procedure(environment, parameterNames, body), environment)
        | procedureExpression :: argumentExpressions ->
            match evaluate procedureExpression environment with
            | Error error -> Error error
            | Ok (Procedure (environment', parameterNames, body), environment'') ->
                evaluateSequence argumentExpressions environment''
                |> Result.bind (fun (values, environment''') ->
                     apply environment' parameterNames body values
                     |> Result.map (fun (value, _) -> value, environment'''))
            | Ok (StandardProcedure procedure, environment'') ->
                evaluateSequence argumentExpressions environment''
                |> Result.bind (fun (values, environment''') ->
                     match procedure values with
                     | Ok value -> Ok(value, environment''')
                     | Error _ -> Error(expression, environment))
            | Ok _ -> Error(expression, environment)
    | Abbreviation (Quote, Expression.List expressions) ->
        evaluateSequence expressions environment
        |> Result.map (fun (values, environment) -> List values, environment)
    | Expression.Vector expressions ->
        evaluateSequence expressions environment
        |> Result.map (fun (values, environment) -> values |> List.toArray |> Vector, environment)
  //| Abbreviation (Quote, expression) -> evaluate expression environment

  and evaluateSequence expressions environment =
    List.fold (fun result expression ->
      match result with
      | Error error -> Error error
      | Ok (values, environment') ->
          evaluate expression environment'
          |> Result.map (fun (value, environment'') -> value :: values, environment'')) (Ok([], environment))
      expressions
    |> Result.map (fun (values, environment') -> List.rev values, environment)

  and apply environment parameterNames body arguments =
    List.zip parameterNames arguments
    |> List.fold (fun environment (parameterName, argument) -> Map.add parameterName argument environment) environment
    |> evaluate body
