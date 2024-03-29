package edu.wpi.rbohrer.piecework

sealed trait AnyShape extends Value

sealed trait Tp {}
case object NumericType extends Tp
case object BoolType extends Tp
case object PointType extends Tp
case object ShapeType extends Tp
case object EdgeType extends Tp
case class ListType(elementType: Tp) extends Tp
case class ProductType(elementTypes: List[Tp]) extends Tp

sealed trait Decl {}
case class FunDecl (name: String, args: List[(String, Tp)], body: Program) extends Decl

sealed trait Program {}
case class Sequence(l: Program, r: Program) extends Program
case class Assign(lhs: Assignable, rhs: Expression) extends Program
case class IfThenElse(cond: Expression, trueBranch: Program, falseBranch: Program) extends Program
case class While(cond: Expression, body: Program) extends Program
case class Return(e: Expression) extends Program


sealed trait Expression extends Program {}
case class Call(name: String, args: List[Expression]) extends Expression
case class Mark(e: Expression, n : Numeric) extends Expression
case class Cut(b: Expression, e: Expression) extends Expression
case class Sew(l: Expression, r: Expression) extends Expression
case class Point(x: Double, y: Double) extends Expression with Value {
  def lerp(other: Point, t: Double): Point = {
    val r = 1.0 - t
    Point(r*x + t*other.x, r*y + t*other.y)
  }
}
case class Edge(beg: Expression, end: Expression) extends Expression with Value
case class Material(r: Double, g: Double, b: Double) extends Expression with Value
case class SimpleShape(edges: List[Expression], mat: Expression) extends Expression with Value with AnyShape {
  def partEdge(e: Edge): (List[Edge],Edge, List[Edge]) = {
    var before: List[Edge] = Nil
    var remain =  edges.asInstanceOf[List[Edge]]
    while(remain.nonEmpty) {
      if(remain.head == e) {
        return (before.reverse, remain.head, remain.tail)
      } else {
        before = remain.head :: before
        remain = remain.tail
      }
    }
    throw new Error("edge not found in simpleshape")
  }
}
case class ComplexShape(shapes: List[Expression], subst: RenamingSubstitution) extends Expression with Value with AnyShape {
}
case class DotX(e: Expression) extends Expression with Numeric
case class DotY(e: Expression) extends Expression with Numeric
case class DotBeg(e: Expression) extends Expression
case class DotEnd(e: Expression) extends Expression
case class DotEdges(e: Expression) extends Expression
case class DotMat(e: Expression) extends Expression
case class DotShapes(e: Expression) extends Expression
case class DotSubst(e: Expression) extends Expression
case class DotLength(e: Expression) extends Expression with Numeric
case class Indexed(e: Expression, n: Numeric) extends Expression

sealed trait Value extends Expression
case class Tuple(xs:List[Value]) extends Value
case object True extends Value
case object False extends Value
case class RenamingSubstitution(l: List[(Variable,Variable)]) extends Value
object RenamingSubstitution {
  def empty: RenamingSubstitution = RenamingSubstitution(Nil)
}

sealed trait Numeric extends Expression {}
case class Number(n : Double) extends Numeric with Value
case class Plus(l: Numeric, r: Numeric) extends Numeric
case class Times(l: Numeric, r: Numeric) extends Numeric
case class Minus(l: Numeric, r: Numeric) extends Numeric
// idx None = normal variable, in scope. idx Some(i) means out-of-scope leftovers, version i.
case class Variable(x : String, idx: Option[Int]) extends Numeric

sealed trait Proposition extends Expression {}
case class Greater(l: Numeric, r: Numeric) extends Proposition
case class Less(l: Numeric, r: Numeric) extends Proposition
case class Equal(l: Numeric, r: Numeric) extends Proposition
case class LessEqual(l: Numeric, r: Numeric) extends Proposition
case class GreaterEqual(l: Numeric, r: Numeric) extends Proposition

sealed trait Assignable {}
case class AssignVar(x: Variable) extends Assignable
case class AssignTuple(asgns : List[Assignable]) extends Assignable
case object Wildcard extends Assignable
