# Copyright (C) 2022 Zachary Guard
# 
# This file is part of zc.
# 
# zc is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# zc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with zc.  If not, see <http://www.gnu.org/licenses/>.

{.experimental: "overloadableEnums".}

import ./nodes
import ./tokenize
import ./parse
import std/[
  math,
  options,
  strutils,
]
from std/typetraits import genericParams, get

export options

type
  Op = enum
    # Call, Infix
    Plus = "+"
    Minus = "-"
    Times = "*"
    Divide = "/"
    Power = "^"
    Sin = "sin"
    Cos = "cos"
    Tan = "tan"
    Asin = "asin"
    Acos = "acos"
    Atan = "atan"
    Abs = "abs"
    Sqrt = "sqrt"
    # Ident
    X = "x"
    E = "e"
    Tau = "tau"
    Pi = "pi"
    # Literal
    Literal
  Instruction = object
    case op: Op
    of Plus..Pi:
      discard
    of Literal:
      value: float
  Expression* = distinct seq[Instruction]

template none(): untyped =
  none(genericParams(typeof result).get(0))

template parseEnum[T: enum](r: Slice[T]; s: string): Option[T] =
  try:
    let e = parseEnum[T](s)
    if e in r:
      e.some
    else:
      T.none
  except ValueError:
    T.none

# compile

func compile(node: Node): Option[Expression] =
  var instructions: seq[Instruction]

  for child in node.children:
    let childInstructions = compile(child)
    if childInstructions.isSome:
      instructions.add(seq[Instruction](childInstructions.get))
    else:
      return none()

  case node.kind
  of Call, Infix:
    let funcName = node.value
    if (let op = parseEnum(Plus..Sqrt, funcName); op.isSome):
      instructions.add Instruction(op: op.unsafeGet)
    else:
      return none()
  of Ident:
    let ident = node.value
    if (let op = parseEnum(X..Pi, ident); op.isSome):
      instructions.add Instruction(op: op.unsafeGet)
    else:
      return none()
  of Literal:
    let value = parseFloat(node.value)
    instructions.add Instruction(op: Literal, value: value)

  Expression(instructions).some

func compile*(expr: string): Option[Expression] =
  let tokens = tokenize(expr)
  if tokens.isSome:
    let tree = parse(tokens.get)
    if tree.isSome:
      result = compile(tree.get)

# evaluate

template uniOp(body: untyped) =
  stack.add(body(stack.pop))

template biOp(body: untyped) =
  let
    b {.inject.} = stack.pop
    a {.inject.} = stack.pop
  stack.add(body)

proc evaluate*(instructions: Expression; argument = 0.0): Option[float] =
  let instructions = seq[Instruction](instructions)

  var stack = @[argument]
  for instruction in instructions:
    case instruction.op
    # Call, Infix
    of Plus: biOp(a + b)
    of Minus: biOp(a - b)
    of Times: biOp(a * b)
    of Divide: biOp(a / b)
    of Power: biOp(a.pow(b))
    of Sin: uniOp(sin)
    of Cos: uniOp(cos)
    of Tan: uniOp(tan)
    of Asin: uniOp(arcsin)
    of Acos: uniOp(arccos)
    of Atan: uniOp(arctan)
    of Abs: uniOp(abs)
    of Sqrt: uniOp(sqrt)
    # Ident
    of X: stack.add(stack[0])
    of E: stack.add(math.E)
    of Tau: stack.add(math.Tau)
    of Pi: stack.add(math.Pi)
    # Literal
    of Literal: stack.add(instruction.value)

  if stack.len >= 1:
    stack.pop.some
  else:
    none()
