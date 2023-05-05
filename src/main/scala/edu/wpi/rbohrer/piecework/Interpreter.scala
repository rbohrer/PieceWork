package edu.wpi.rbohrer.piecework

import geny.Generator.from

import scala.collection.IterableOnce.iterableOnceExtensionMethods

object Interpreter {

  private def assignTo(assignable: Assignable, v: Value, s: State): (Value, State) = {
    assignable match {
      case AssignVar(x) => (v, s.updateVar(x.x,v))
      case AssignTuple(asgns: List[Assignable]) =>
        val Tuple(vs: List[Value]) = v
        (v,asgns.zip(vs).foldLeft(s){case (s,(a,v)) => assignTo(a,v,s)._2})
      case Wildcard => (v,s)
    }
  }

  def apply(e: Program, s:State): (Value, State) = {
    e match {
      case Sequence(l, r) =>
        val (v1,s1) = apply(l,s)
        apply(r, s1)
      case Assign(lhs, rhs) =>
        val (v1,s1) = apply(rhs,s)
        assignTo(lhs, v1, s1)
      case IfThenElse(cond, trueBranch, falseBranch) =>
        val (v1,s1) =apply(cond,s)
        v1 match {
          case True => apply(trueBranch, s1)
          case _  => apply(falseBranch, s1)
        }
      case While(cond, body) =>
        var (v1, s1) = apply(cond,s)
        while(v1 == True) {
          val (v2, s2) = apply(cond,s)
          v1 = v2
          s1 = s2
        }
        (v1,s1)
      case Return(e) => apply(e,s)
      case expression: Expression =>
        expression match {
          case Call(name, args) => ???
          case Mark(e, n) => ???
          case Cut(b, e) => ???
          case Sew(l, r) => ???
          case p: Point => (p,s)
          case m: Material => (m,s)
          case cs: ComplexShape => (cs,s)
          case Edge(beg, end) =>
            val (v1 : Point, s1) = apply(beg,s)
            val (v2: Point, s2) = apply(end, s1)
            (Edge(v1,v2),s2)

          case SimpleShape(edges, mat) =>
            val (vs:List[Value], s1) =
              edges.foldLeft((Nil: List[Value],s))({case((vs,s),e) =>
              val (v1,s1) = apply(e,s)
              (v1 :: vs, s1)
            })
            val (m, s2) = apply(mat, s1)
            (SimpleShape(vs.reverse, m),s2)
          case DotX(e) =>
            val (Point(x,y), s1) = apply(e,s); (Number(x),s1)
          case DotY(e) =>
            val (Point(x,y), s1) = apply(e,s); (Number(y),s1)
          case DotBeg(e) =>
            val (Edge(beg:Point,end), s1) = apply(e,s); (beg,s1)
          case DotEnd(e) =>
            val (Edge(beg,end:Point), s1) = apply(e,s); (end,s1)
          case DotEdges(e) =>
            val (SimpleShape(es:List[Edge],_m), s1) = apply(e,s); (Tuple(es), s1)
          case DotMat(e) =>
            val (SimpleShape(_es,m:Material), s1) = apply(e,s); (m, s1)
          case DotShapes(e) =>
            val (ComplexShape(ss, _sub), s1) = apply(e,s); (Tuple(ss), s1)
          case DotSubst(e) =>
            val (ComplexShape(_ss, sub), s1) = apply(e,s); (sub, s1)
          case DotLength(e) =>
            val (Tuple(vs), s1) = apply(e,s); (Number(vs.length.toDouble),s1)
          case Indexed(e, n) =>
            val (Tuple(vs), s1) = apply(e,s)
            val (n1:Number, s2) = apply(n, s1)
            (vs(n1.n.toInt),s2)
          case numeric: Numeric =>
            numeric match {
              case n: Number =>(n,s)
              case Plus(l, r) =>
                val (l:Number, s1) = apply(l, s)
                val (r:Number, s2) = apply(r,s1)
                (Number(l.n+r.n),s2)
              case Times(l, r) =>
                val (l:Number, s1) = apply(l, s)
                val (r:Number, s2) = apply(r,s1)
                (Number(l.n*r.n),s2)
              case Minus(l, r) =>
                val (l:Number, s1) = apply(l, s)
                val (r:Number, s2) = apply(r,s1)
                (Number(l.n-r.n),s2)
              case Variable(x) => (s.getVar(x),s)

            }
          case proposition: Proposition =>
            def doBool(l: Expression, r: Expression, f: (Double,Double) => Boolean): (Value, State) = {
              val (v1: Number, s1) = apply(l,s)
              val (v2: Number, s2) = apply(r,s1)
              (if(f(v1.n,v2.n)) True else False, s2)
            }
            proposition match {
              case Greater(l, r) =>
                doBool(l,r, ((x,y) => x > y))
              case Less(l, r) =>
                doBool(l,r, ((x,y) => x < y))
              case Equal(l, r) =>
                doBool(l,r, ((x,y) => x == y))
              case LessEqual(l, r) =>
                doBool(l,r, ((x,y) => x <= y))
              case GreaterEqual(l, r) =>
                doBool(l,r, ((x,y) => x >= y))
            }
        }
    }
  }

  def apply(e: List[FunDecl]): (Value, State) = {
    val s = State.empty.addDecls(e)
    apply(s.main.body, s)
  }
}
