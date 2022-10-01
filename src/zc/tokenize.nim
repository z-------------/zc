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

import ./tokens
import std/[
  options,
  strutils,
  typetraits,
]

template none(): untyped =
  none(genericParams(typeof result).get(0))

type
  TokenizerState = object
    expr: string
    pos: Natural
  SingleTokenizer = proc (s: var TokenizerState): Option[Token] {.nimcall.}

func tryTokenize(s: var TokenizerState; singleTokenizer: SingleTokenizer): Option[Token] =
  let initialPos = s.pos
  result = singleTokenizer(s)
  if result.isNone:
    s.pos = initialPos

func oneOf(s: var TokenizerState; singleTokenizers: varargs[SingleTokenizer]): Option[Token] =
  for singleTokenizer in singleTokenizers:
    let singleResult = tryTokenize(s, singleTokenizer)
    if singleResult.isSome:
      return singleResult
  none()

func readChar(s: var TokenizerState): char =
  result =
    if s.pos < s.expr.len:
      s.expr[s.pos]
    else:
      '\0'
  inc s.pos

# Operator

func readOperator(s: var TokenizerState): Option[Token] =
  let c = readChar(s)
  if c in {'+', '-', '*', '/', '^'}:
    initToken(Operator, $c).some
  else:
    none()

# Literal

func readLiteral(s: var TokenizerState): Option[Token] =
  var literal = ""
  var
    isDone = false
    isDotAllowed = true
  while not isDone and s.pos < s.expr.len:
    let c = readChar(s)
    if c.isDigit:
      literal.add(c)
    elif isDotAllowed and c == '.':
      literal.add('.')
      isDotAllowed = false
    else:
      dec s.pos
      isDone = true
  if literal != "":
    initToken(Literal, literal).some
  else:
    none()

# Identifier

template isIdentChar(c: char): bool =
  c in {'A'..'Z', 'a'..'z'}

func readIdent(s: var TokenizerState): Option[Token] =
  var ident = ""
  var c: char
  while (c = readChar(s); c).isIdentChar:
    ident.add(c)
  dec s.pos
  if ident != "":
    initToken(Ident, ident).some
  else:
    none()

# ParenOpen, ParenClose

func readParen(s: var TokenizerState): Option[Token] =
  case readChar(s)
  of '(':
    initToken(ParenOpen).some
  of ')':
    initToken(ParenClose).some
  else:
    none()

# root

func tokenize(s: var TokenizerState): Option[seq[Token]] =
  var tokens: seq[Token]
  while s.pos < s.expr.len:
    let singleResult = s.oneOf(readOperator, readLiteral, readIdent, readParen)
    if singleResult.isNone:
      return none()
    else:
      tokens.add(singleResult.get)
    if s.pos < s.expr.len:
      while s.expr[s.pos] == ' ':
        inc s.pos
  if s.pos == s.expr.len:
    tokens.some
  else:
    none()

func tokenize*(expr: sink string): Option[seq[Token]] =
  var state = TokenizerState(
    expr: expr,
    pos: 0,
  )
  tokenize(state)
