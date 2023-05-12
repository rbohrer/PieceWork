package edu.wpi.rbohrer.piecework.penrose

import edu.wpi.rbohrer.piecework._
import penrose.Sequence

object PenroseConverter {
  val DOMAIN = "mesh-set-domain"
  val DEFAULT_EDGE_NAME = "e"
  val DEFAULT_POINT_NAME = "x"

  var pointMap:Map[Point, String] = Map()
  var edgeMap:Map[Edge, String] = Map()
  def recordEdge(n: String, e: Edge): String = {
    edgeMap = edgeMap + (e -> n)
    n
  }
  def recordPoint(n: String, p: Point): String = {
    pointMap = pointMap + (p -> n)
    n
  }
  def nameEdge(e: Edge): String = {
    if (edgeMap.contains(e)) {
      edgeMap(e)
    } else {
      val i = edgeMap.size
      val name = DEFAULT_EDGE_NAME + i
      edgeMap = edgeMap + (e -> name)
      name
    }
  }
  def namePoint(p: Point): String = {
    if (pointMap.contains(p)) {
      pointMap(p)
    } else {
      val i = pointMap.size
      val name = DEFAULT_POINT_NAME + i
      pointMap = pointMap + (p -> name)
      name
    }
  }

  def substance(name: String, ss: SimpleShape): PenroseExpression = {
    def consOpt(opt: Option[PenroseExpression] , triangle: Triangle): Option[PenroseExpression] = {
      opt match {
        case None => Some(triangle)
        case Some(penrose.Sequence(es)) => Some(Sequence( triangle :: es))
        case Some(e) => Some(Sequence(triangle :: e :: Nil))
      }
    }
    var i = 0
//    val edge
    var edgeBuf = ss.edges.map(_.asInstanceOf[Edge])
    // decompose polygon into fan shape
    var root = edgeBuf.head
    edgeBuf = edgeBuf.tail
    var shExpr:Option[PenroseExpression] = None
    while(edgeBuf.length > 1) {
      val (ea,eb,ec) = (nameEdge(root), nameEdge(edgeBuf(0)),nameEdge(edgeBuf(1)))
      val (xa,xb,xc) = (namePoint(root.beg.asInstanceOf[Point]), namePoint(edgeBuf(0).beg.asInstanceOf[Point]),namePoint(edgeBuf(1).beg.asInstanceOf[Point]))
      val triName = if(i == 0) name else name + i
      shExpr = consOpt(shExpr, Triangle(triName,xa,xb,xc))
      // Drop second element
      edgeBuf = edgeBuf.tail
      i = i + 1
    }
    shExpr.get
  }

  def substance(name: String, cs: ComplexShape): PenroseExpression = {
      val sub = cs.subst
      val shapes = cs.shapes.zipWithIndex.map({case (n, i) => (name + i, n)})
      val labName = shapes.head._1
      Sequence(shapes.map({case(x,y)=>substance(x,y)}) :+ Label(labName, name))
  }

  def namedEdgeSubstance: PenroseExpression = {
    val edgs  = edgeMap.toList.map({case (e, n) =>
      penrose.Edge(n, namePoint(e.beg.asInstanceOf[Point]), namePoint(e.end.asInstanceOf[Point]))})
    val pts = pointMap.toList.map({case (p,n) => Vertex(n)})
    Sequence(pts ++ edgs)
  }

  /** Generates contents of Penrose substance file
   * corresponding to given state of program */
  def substance(s: State): PenroseExpression = {
    val map: Map[String,Value] = s.simpleEnv
    val alist = map.toList
    val css: List[(String,ComplexShape)] = alist.filter(_._2.isInstanceOf[ComplexShape]).map({case (x,y:ComplexShape) => (x,y)})
    val sss: List[(String,SimpleShape)] = alist.filter(_._2.isInstanceOf[SimpleShape]).map({case (x,y:SimpleShape) => (x,y)})
    val edges: List[(String,Edge)] = alist.filter(_._2.isInstanceOf[Edge]).map({case (x,y:Edge) => (x,y)})
    val vertices: List[(String,Point)] = alist.filter(_._2.isInstanceOf[Point]).map({case (x,y:Point) => (x,y)})
    // @TODO: Generate Penrose code!
    edges.map({case (x,y) => recordEdge(x,y)})
    // Let's first try generating just the complex shapes
    val ms = css.map({case ((x,y)) => substance(x,y)})
    val nes = namedEdgeSubstance
    Sequence(nes :: ms)

  }
}
