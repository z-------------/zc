import ./zc/[
  expressions,
]
import std/[
  options,
]

export expressions

proc evaluate*(expression: string; argument = 0.0): Option[float] =
  compile(expression).flatMap do (compiledExpression: Expression) -> Option[float]:
    evaluate(compiledExpression, argument)
