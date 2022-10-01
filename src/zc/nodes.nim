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
  strutils,
]

type
  NodeKind* = enum
    Infix
    Literal
    Ident
    Call
  Node* = ref object
    kind*: NodeKind
    value*: string
    children*: seq[Node]

func newNode*(kind: NodeKind; value: sink string; children: sink seq[Node] = @[]): Node =
  Node(
    kind: kind,
    value: value,
    children: children,
  )

func toString(node: Node; indent = 0): string =
  let indentStr = "  ".repeat(indent)
  result &= indentStr
  result &= $node.kind
  if node.value != "":
    result &= ' '
    result &= node.value
  for child in node.children:
    result &= '\n'
    result &= child.toString(indent + 1)

func `$`*(node: Node): string =
  if node.isNil:
    "nil"
  else:
    node.toString

func render*(node: Node): string =
  case node.kind
  of Infix:
    &"({node.children[0].render} {node.value} {node.children[1].render})"
  of Ident, Literal:
    $node.value
  of Call:
    &"{node.value} ({node.children[0].render})"
