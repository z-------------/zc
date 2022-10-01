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
import ./tokens
import std/[
  options,
  sugar,
  typetraits,
]

template none(): untyped =
  none(genericParams(typeof result).get(0))

type
  ParserState = object
    tokens: seq[Token]
    pos: Natural
  SingleParser = proc (s: var ParserState): Option[Node] {.nimcall.}

func getToken(s: var ParserState): Option[Token] =
  if s.pos >= s.tokens.len:
    none()
  else:
    s.tokens[s.pos].some

func readToken(s: var ParserState; kind: TokenKind): Option[Token] =
  let token = getToken(s)
  if token.isSome and token.get.kind == kind:
    inc s.pos
    token
  else:
    none()

func parseLiteral(s: var ParserState): Option[Node] =
  s.readToken(Literal).map(token => newNode(Literal, token.value))

func parseIdent(s: var ParserState): Option[Node] =
  s.readToken(Ident).map(token => newNode(Ident, token.value))

func parseExpression(s: var ParserState): Option[Node]

func parseParen(s: var ParserState): Option[Node] =
  let initialPos = s.pos
  if s.readToken(ParenOpen).isSome:
    let expr = parseExpression(s)
    if expr.isSome and s.readToken(ParenClose).isSome:
      result = expr
  if result.isNone:
    s.pos = initialPos

func parseCall(s: var ParserState): Option[Node] =
  let initialPos = s.pos
  let identToken = s.readToken(Ident)
  if identToken.isNone:
    return none()
  let argNode = parseParen(s)
  if argNode.isNone:
    s.pos = initialPos
    return none()
  newNode(Call, identToken.get.value, @[argNode.get]).option

func parseAtom(s: var ParserState): Option[Node] =
  if result.isNone:
    result = parseLiteral(s)
  if result.isNone:
    result = parseCall(s)
  if result.isNone:
    result = parseIdent(s)
  if result.isNone:
    result = parseParen(s)

func parseInfix(s: var ParserState; operators: openArray[Token]; initialNode: Node; childParser: SingleParser): Option[Node] =
  ## Parse an infix expression.
  ## operators - The set of operators that separate operands at this level of precedence.
  ## initialNode
  ## childParser - The parser for expressions immediately above this one in the precedence hierarchy.
  ## Returns the root node for the infix expression.
  var node = initialNode

  let operand = childParser(s)
  if operand.isNone:
    return none()
  elif not node.isNil:
    node.children.add(operand.get)
  else:
    node = operand.get

  var token: Option[Token]
  while (token = getToken(s); token).isSome and token.get in operators:
    inc s.pos
    let newRoot = newNode(Infix, token.get.value)
    newRoot.children.add(node)
    let operand = childParser(s)
    if operand.isNone:
      return none()
    else:
      newRoot.children.add(operand.get)
    node = newRoot

  option(node)

func parseInfixExponentiative(s: var ParserState): Option[Node] =
  const Operators = [initOperator("^")]
  s.parseInfix(Operators, nil, parseAtom)

func parseInfixMultiplicative(s: var ParserState): Option[Node] =
  const Operators = [initOperator("*"), initOperator("/")]
  s.parseInfix(Operators, nil, parseInfixExponentiative)

func parseInfixAdditive(s: var ParserState): Option[Node] =
  const Operators = [initOperator("+"), initOperator("-")]

  var node: Node
  let token = getToken(s)
  if token.isSome and token.get in Operators:
    inc s.pos
    node = newNode(Infix, token.get.value)
    node.children.add(newNode(Literal, "0"))

  s.parseInfix(Operators, node, parseInfixMultiplicative)

func parseExpression(s: var ParserState): Option[Node] =
  parseInfixAdditive(s)

func parse(s: var ParserState): Option[Node] =
  parseExpression(s)

func parse*(tokens: sink seq[Token]): Option[Node] =
  var state = ParserState(
    tokens: tokens,
    pos: 0,
  )
  parse(state)
