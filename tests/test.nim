import zc

import std/[
  unittest,
  math,
]

template checkExprValue(expr: string; argument: float; expected: float): untyped =
  let val = evaluate(expr, argument)
  check val.isSome
  if val.isSome:
    check val.unsafeGet.almostEqual(expected)

template checkExprValue(expr: string; expected: float): untyped =
  checkExprValue(expr, 0.0, expected)

test "it works":
  checkExprValue "2.17 * sin(pi + sqrt(4))":
    2.17 * sin(Pi + sqrt(4.0))

  const x = -50.5
  checkExprValue "2.17 * tan(x + 3 * x / sin(x ^ 2))", x:
    2.17 * tan(x + ((3 * x) / sin(x ^ 2)))

test "operations return correct values":
  let pi = Pi # black magic fuckery happens if you directly use Pi

  checkExprValue "69 + 420", 69 + 420
  checkExprValue "69 - 420", 69 - 420
  checkExprValue "69 * 420", 69 * 420
  checkExprValue "69 / 420", 69 / 420
  checkExprValue "69 ^ 420", 69.0.pow(420.0)
  checkExprValue "sin(pi * 1.5)", sin(pi * 1.5)
  checkExprValue "cos(pi * 1.5)", cos(pi * 1.5)
  checkExprValue "tan(pi * 1.5)", tan(pi * 1.5)
  checkExprValue "asin(0.69)", arcsin(0.69)
  checkExprValue "acos(0.69)", arccos(0.69)
  checkExprValue "atan(0.69)", arctan(0.69)
  checkExprValue "abs(69 - 420)", abs(69 - 420)
  checkExprValue "sqrt(420)", sqrt(420.0)

test "idents work":
  const x = -50.5

  checkExprValue "x", x, x
  checkExprValue "e", math.E
  checkExprValue "tau", math.Tau
  checkExprValue "pi", math.Pi

test "error on invalid input":
  check compile"pi(2)".isNone
  check evaluate"pi(2)".isNone
