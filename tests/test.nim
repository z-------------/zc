import zc

import std/[
  unittest,
  math,
]

test "it works":
  check evaluate("""2.17 * sin(pi + sqrt(4))""").get.almostEqual(2.17 * sin(Pi + sqrt(4.0)))

  const x = -50.5
  check evaluate("""2.17 * tan(x + 3 * x / sin(x ^ 2))""", x).get.almostEqual(2.17 * tan(x + ((3 * x) / sin(x ^ 2))))

test "operations return correct values":
  let pi = Pi # black magic fuckery happens if you directly use Pi

  check evaluate("""69 + 420""").get.almostEqual(69 + 420)
  check evaluate("""69 - 420""").get.almostEqual(69 - 420)
  check evaluate("""69 * 420""").get.almostEqual(69 * 420)
  check evaluate("""69 / 420""").get.almostEqual(69 / 420)
  check evaluate("""69 ^ 420""").get.almostEqual(69.0.pow(420.0))
  check evaluate("""sin(pi * 1.5)""").get.almostEqual(sin(pi * 1.5))
  check evaluate("""cos(pi * 1.5)""").get.almostEqual(cos(pi * 1.5))
  check evaluate("""tan(pi * 1.5)""").get.almostEqual(tan(pi * 1.5))
  check evaluate("""asin(0.69)""").get.almostEqual(arcsin(0.69))
  check evaluate("""acos(0.69)""").get.almostEqual(arccos(0.69))
  check evaluate("""atan(0.69)""").get.almostEqual(arctan(0.69))
  check evaluate("""abs(69 - 420)""").get.almostEqual(abs(69 - 420))
  check evaluate("""sqrt(420)""").get.almostEqual(sqrt(420.0))

test "idents work":
  const x = -50.5

  check evaluate("x", x).get.almostEqual(x)
  check evaluate("e").get.almostEqual(math.E)
  check evaluate("tau").get.almostEqual(math.Tau)
  check evaluate("pi").get.almostEqual(math.Pi)
