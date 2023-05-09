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

  // Helper functions defined as in MS thesis
  private def mrk(ss: SimpleShape, e: Edge, r: Double): (SimpleShape, Point) = {
    val mid = e.beg.asInstanceOf[Point].lerp(e.end.asInstanceOf[Point],r)
    val (before, at, after) = ss.partEdge(e)
    (SimpleShape(before ++ (Edge(at.beg,mid) :: Edge(mid,at.end) :: after), ss.mat), mid)
  }

  private def ct(sh: SimpleShape, edg1: Edge, edg2: Edge): (SimpleShape, SimpleShape) = {
    val (ea, at1, after1) = sh.partEdge(edg1)
    val (eb, at2, ec) = SimpleShape(after1, sh.mat).partEdge(edg2)
    val s1 = SimpleShape(ea ++ (at1 :: Edge(edg1.end, edg2.end) :: ec), sh.mat)
    val s2 = SimpleShape(eb ++ (at2 :: Edge(edg2.end, edg1.end) :: Nil), sh.mat)
    (s1,s2)
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
      case Return(e) =>
        val (v, s1) = apply(e,s)
        (v, s1.ret)
      case expression: Expression =>
        expression match {
          case Mark(e, n) =>
            val (v1: Edge,s1) = apply(e, s)
            val (v2: Number, s2) = apply(n,s1)
            // TODO: assert both points are on the same shape
            val (key, shape, shapes, sub) = s2.locateEdge(v1)
            val (newShape,mid) = mrk(shape,v1,v2.n)
            val cs = ComplexShape(newShape :: shapes, sub)
            val s3 = s2.updateShapeInPlace(key,cs)
            (mid, s3)
          case Cut(b, e) =>
            val (vb : Point, s1) = apply(b,s)
            val (ve : Point, s2) = apply(e,s1)
            val edgB = s2.locateByEnd(vb)
            val edgE = s2.locateByEnd(ve)
            // @TODO: Check same shape
            val (key, shape, shapes, sub) = s.locateEdge(edgB)
            val (sh1,sh2) = ct(shape,edgB,edgE)
            val s3 = s2.updateShapeInPlace(key,sh1)
            val s4 = s3.addShapeToComplex(key, sh2)
            (s.getVar(key.x), s4)
          case Sew(l, r) =>
            val (vl:Edge, s1) = apply(l, s)
            val (vr:Edge, s2) = apply(r, s1)
            val (ne1, ne2) = (s2.nameOf(vl), s2.nameOf(vr))
            val (ne1b, ne1e) = (s2.nameOf(vl.beg.asInstanceOf[Value]), s2.nameOf(vl.end.asInstanceOf[Value]))
            val (ne2b, ne2e) = (s2.nameOf(vr.beg.asInstanceOf[Value]), s2.nameOf(vr.end.asInstanceOf[Value]))
            val newSub = RenamingSubstitution((ne1,ne2) :: (ne1b, ne2b) :: (ne1e, ne2e) :: Nil)
            (newSub, s.applySub(newSub))
            // @TODO the substitution
          case Call(name, args) =>
            val fd: FunDecl = s.findFunction(name)
            val (vs,s1) = args.foldLeft((Nil:List[Value],s)){case ((vs,s),e) =>
              val (v1,s1) = apply(e,s)
              (v1 :: vs, s1)
            }
            val vals = vs.reverse
            val s2 = s1.call(fd.args.map(_._1), vals)
            apply(fd.body, s2)
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
            apply(e,s) match {
              case (SimpleShape(es:List[Edge],_m), s1) =>  (Tuple(es), s1)
              case (ComplexShape(SimpleShape(es:List[Edge],_m) :: _, sub), s1) =>  (Tuple(es), s1)
            }
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
                val (nl:Number, s1) = apply(l, s)
                val (nr:Number, s2) = apply(r,s1)
                (Number(nl.n+nr.n),s2)
              case Times(l, r) =>
                val (nl:Number, s1) = apply(l, s)
                val (nr:Number, s2) = apply(r,s1)
                (Number(nl.n*nr.n),s2)
              case Minus(l, r) =>
                val (nl:Number, s1) = apply(l, s)
                val (nr:Number, s2) = apply(r,s1)
                (Number(nl.n-nr.n),s2)
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

  def apply(e: List[Decl]): (Value, State) = {
    val s = State.empty.addDecls(e)
    apply(s.main.body, s)
  }
}
