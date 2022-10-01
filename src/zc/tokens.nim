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

import std/[
  strformat,
]

type
  TokenKind* = enum
    Operator
    Literal
    Ident
    ParenOpen
    ParenClose
  Token* = object
    kind*: TokenKind
    value*: string

func initToken*(kind: TokenKind; value = ""): Token =
  Token(
    kind: kind,
    value: value,
  )

func initOperator*(value: string or char): Token =
  initToken(Operator, $value)

func `$`*(token: Token): string =
  result &= &"Token({token.kind}"
  if token.value != "":
    result &= &", \"{token.value}\""
  result &= ")"
