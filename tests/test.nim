import zc

import std/[
  unittest,
  math,
]

test "it works":
  check evaluate("""2.17 * sin(pi + sqrt(4))""").get.almostEqual(2.17 * sin(Pi + sqrt(4.0)))

  const x = -50.5
  check evaluate("""2.17 * tan(x + 3 * x / sin(x ^ 2))""", x).get.almostEqual(2.17 * tan(x + ((3 * x) / sin(x ^ 2))))
