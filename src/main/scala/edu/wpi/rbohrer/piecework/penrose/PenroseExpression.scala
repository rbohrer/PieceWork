package edu.wpi.rbohrer.piecework.penrose

/** Custom AST to be exported to Penrose syntax  */
sealed trait PenroseExpression {}
//case class Complex(name: String)
case class Vertex(name: String) extends PenroseExpression {
  override def toString: String = {
    "Vertex " + name
  }
}
case class Edge(name: String, beg: String, end: String) extends PenroseExpression {
  override def toString: String = {
    s"Edge $name := MakeEdge($beg,$end)"
  }
}
case class Triangle(name: String, a: String, b: String, c: String) extends PenroseExpression {
  override def toString: String = {
    s"Triangle $name := MakeTriangle($a,$b,$c)"
  }
}
case class Label(target: String, content: String) extends PenroseExpression {
  override def toString: String = {
    s"""Label $target "$content""""
  }
}
case class Sequence(pes: List[PenroseExpression]) extends PenroseExpression {
  override def toString: String = {
    pes.mkString("\n")
  }
}
