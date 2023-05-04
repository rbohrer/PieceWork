package edu.wpi.rbohrer.piecework


case class RenamingSubstitution(l: List[(String,String)])

sealed trait Tp {}
case object NumericType extends Tp
case object BoolType extends Tp
case object PointType extends Tp
case object ShapeType extends Tp
case object EdgeType extends Tp
case class ListType(elementType: Tp) extends Tp


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
case class Point(x: Double, y: Double) extends Expression
case class Edge(beg: Expression, end: Expression) extends Expression
case class Material(r: Double, g: Double, b: Double) extends Expression
case class SimpleShape(edges: List[Expression], mat: Expression) extends Expression
case class ComplexShape(shapes: List[SimpleShape], subst: RenamingSubstitution) extends Expression
case class DotX(e: Expression) extends Expression
case class DotY(e: Expression) extends Expression
case class DotBeg(e: Expression) extends Expression
case class DotEnd(e: Expression) extends Expression
case class DotEdges(e: Expression) extends Expression
case class DotMat(e: Expression) extends Expression
case class DotShapes(e: Expression) extends Expression
case class DotSubst(e: Expression) extends Expression
case class Indexed(e: Expression, n: Numeric) extends Expression

sealed trait Numeric extends Expression {}
case class Number(n : Double) extends Numeric
case class Plus(l: Numeric, r: Numeric) extends Numeric
case class Times(l: Numeric, r: Numeric) extends Numeric
case class Minus(l: Numeric, r: Numeric) extends Numeric
case class Variable(x : String) extends Numeric

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
