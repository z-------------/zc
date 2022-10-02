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
  math,
  options,
  strutils,
  typetraits,
]

export options

type
  Instruction = proc (stack: var seq[float])
  Expression* = distinct seq[Instruction]

template none(): untyped =
  none(genericParams(typeof result).get(0))

# instructions

template uniFunc(expr: untyped): Instruction =
  (func (stack: var seq[float]) =
    let a = stack.pop
    stack.add(expr(a))
  )

template biFunc(expr: untyped): Instruction =
  (func (stack: var seq[float]) =
    let
      b {.inject.} = stack.pop
      a {.inject.} = stack.pop
    stack.add(expr)
  )

template pushVal(value: float): Instruction =
  (func (stack: var seq[float]) =
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
      of "+": biFunc(a + b)
      of "-": biFunc(a - b)
      of "*": biFunc(a * b)
      of "/": biFunc(a / b)
      of "^": biFunc(a.pow(b))
      of "sin": uniFunc(sin)
      of "cos": uniFunc(cos)
      of "tan": uniFunc(tan)
      of "abs": uniFunc(abs)
      of "sqrt": uniFunc(sqrt)
      else:
        return none()
  of Ident:
    let ident = node.value
    instructions.add:
      case ident
      of "x": (func (stack: var seq[float]) = stack.add(stack[0]))
      of "e": pushVal(math.E)
      of "tau": pushVal(math.Tau)
      of "pi": pushVal(math.Pi)
      else:
        return none()
  of Literal:
    let value = parseFloat(node.value)
    instructions.add(pushVal(value))

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
  for instruction in instructions:
    instruction(stack)

  if stack.len >= 1:
    stack.pop.some
  else:
    none()
