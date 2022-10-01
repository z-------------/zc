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

import ./nodes
import ./tokenize
import ./parse
import std/[
  options,
  typetraits,
  sugar,
  math,
  strutils,
]

export options

type
  Instruction = proc (stack: var seq[float])
  Expression* = distinct seq[Instruction]

template none(): untyped =
  none(genericParams(typeof result).get(0))

func uniFunc(stack: var seq[float]; f: proc (a: float): float) =
  stack.add(f(stack.pop))

func biFunc(stack: var seq[float]; f: proc (a, b: float): float) =
  let
    b = stack.pop
    a = stack.pop
  stack.add(f(a, b))

# instructions

func instructionAdd(stack: var seq[float]) =
  biFunc(stack, (a, b) => a + b)

func instructionSubtract(stack: var seq[float]) =
  biFunc(stack, (a, b) => a - b)

func instructionMultiply(stack: var seq[float]) =
  biFunc(stack, (a, b) => a * b)

func instructionDivide(stack: var seq[float]) =
  biFunc(stack, (a, b) => (if b == 0: float.high else: a / b))

func instructionPower(stack: var seq[float]) =
  biFunc(stack, pow)

func instructionSin(stack: var seq[float]) =
  uniFunc(stack, sin)

func instructionCos(stack: var seq[float]) =
  uniFunc(stack, cos)

func instructionTan(stack: var seq[float]) =
  uniFunc(stack, tan)

func instructionAbs(stack: var seq[float]) =
  uniFunc(stack, a => abs(a))

func instructionSqrt(stack: var seq[float]) =
  uniFunc(stack, sqrt)

func instructionPushArgument(stack: var seq[float]) =
  stack.add(stack[0])

func getInstructionPush(value: float): proc (stack: var seq[float]) =
  (proc (stack: var seq[float]) =
    stack.add(value)
  )

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
    instructions.add:
      case funcName
      of "+": instructionAdd
      of "-": instructionSubtract
      of "*": instructionMultiply
      of "/": instructionDivide
      of "^": instructionPower
      of "sin": instructionSin
      of "cos": instructionCos
      of "tan": instructionTan
      of "abs": instructionAbs
      of "sqrt": instructionSqrt
      else:
        return none()
  of Ident:
    let ident = node.value
    instructions.add:
      case ident
      of "x": instructionPushArgument
      of "e": getInstructionPush(math.E)
      of "tau": getInstructionPush(math.Tau)
      of "pi": getInstructionPush(math.Pi)
      else:
        return none()
  of Literal:
    let value = parseFloat(node.value)
    instructions.add(getInstructionPush(value))

  Expression(instructions).some

func compile*(expr: string): Option[Expression] =
  let tokens = tokenize(expr)
  if tokens.isSome:
    let tree = parse(tokens.get)
    if tree.isSome:
      result = compile(tree.get)

# evaluate

proc evaluate*(instructions: Expression; argument = 0.0): Option[float] =
  let instructions = seq[Instruction](instructions)

  var stack = @[argument]
  var programCounter = 0

  while programCounter < instructions.len:
    instructions[programCounter](stack)
    inc programCounter

  if stack.len >= 1:
    stack.pop.some
  else:
    none()
