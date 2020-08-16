namespace R7rsSmall.Tests

open Expecto
open R7rsSmall

module Evaluator =

  let evaluateProcedureApplicationTest =
    testCase "Evaluator correctly evaluates procedure application"
    <| fun _ ->
      Expect.equal
        (Evaluator.evaluate
          (List [ List [ Symbol "lambda"
                         List [ Symbol "x" ]
                         Symbol "x" ]
                  Expression.Boolean true ])
           Map.empty)
        (Ok(Value.Boolean true, Map.empty))
        ""

  [<Tests>]
  let tests =
    testList "Evaluator tests" [ evaluateProcedureApplicationTest ]
