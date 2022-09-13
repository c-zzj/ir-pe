package pe

import ast.Exp

object DeadCodeElim:
  def visit(e: Exp): Exp = e


