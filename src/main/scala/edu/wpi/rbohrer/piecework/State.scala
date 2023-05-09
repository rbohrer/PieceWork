package edu.wpi.rbohrer.piecework

case class State(var decls: List[Decl], var env: List[Map[String,Value]], var space: Map[String,List[Value]]) {
  def addDecls(ds: List[Decl]): State = State(decls ++ ds, env, space)
  // N.B. updateVar can only be applied to live variables, not dead, so String vs Variable here.
  def updateVar(x: String, v: Value): State = {
    env match {
      case e:: es =>
        val e1 = e.+(x -> v)
        State(decls, e1 :: es, space)
      case Nil =>
        State(decls, Map(x -> v) :: Nil, space)
    }
  }

  // applies to both live and dead variables
  private def getLiveVar(x: String): Value = {
    var envs = env
    while(true) {
      if(envs.head.contains(x)) {
        return envs.head(x)
      } else {
        envs = envs.tail
      }
    }
    throw new Error("unreachable")
  }

  private def getDeadVar(str: String, i: Int): Value = {
    val list = space(str)
    list(list.length -  (i+1))
  }

  def getVar(v: Variable): Value  = {
    v match {
      case Variable(x, None) => getLiveVar(x)
      case Variable(x, Some(i)) => getDeadVar(x,i)
    }

  }

  def main: FunDecl = decls.last.asInstanceOf[FunDecl]
  def call(xs: List[String], vs: List[Value]): State = {
    val argEnv = xs.zip(vs).toMap
    State(decls, argEnv::env, space)
  }

  def ghostEdge(e: Edge): State = {
    State(decls,env,saveVar(space, "ghostEdges", e))
  }
  def ghostEdges(es: List[Edge]): State = {
    es.foldLeft(this)({case (acc, e) => acc.ghostEdge(e)})
  }

  private def saveVar(space: Map[String,List[Value]], k: String, v: Value): Map[String,List[Value]] = {
    if (space.contains(k))
      space + (k -> (v :: space(k)))
    else
      space + (k -> (v :: Nil))
  }

  private def extendSpace(space: Map[String,List[Value]], frame: Map[String,Value]): Map[String,List[Value]] = {
    frame.toList.foldLeft(space)({case (acc, (k,v)) => saveVar(acc, k, v)})
  }

  // @TODO: Move current
  def ret: State = {
    State(decls, env.tail, extendSpace(space, env.head))
  }

  def findFunction(name: String): FunDecl = {
    decls.find({case fd : FunDecl if fd.name == name => true}).get.asInstanceOf[FunDecl]
  }

  def bindData[T](f: (Variable, Value) => Option[T]): Option[T] = {
    bindEnv({case (k,v) => f(Variable(k, None), v)}) match {
      case Some(x) => Some(x)
      case None => bindSpace({case (k,v,i) => f(Variable(k,Some(i)), v)})
    }
  }

  def bindSpace[T](f: (String,Value,Int) => Option[T]): Option[T] = {
    var thisEnv = space.toList
    while(thisEnv.nonEmpty) {
      val (k,vs) = thisEnv.head
      vs.zipWithIndex.find({case (v,i) => f(k,v,i).isDefined}) match {
        case Some((v,i)) => return f(k,v,i)
        case None => thisEnv = thisEnv.tail
      }
    }
    return None
  }

  def bindEnv[T](f: (String,Value) => Option[T]): Option[T] = {
    var envs = env
    while(envs.nonEmpty) {
      var thisEnv = envs.head.toList
      while(thisEnv.nonEmpty)
      {
        val (k,v) = thisEnv.head
        f(k,v) match {
          case Some(z) =>
            return Some(z)
          case _ =>
            thisEnv = thisEnv.tail
        }
      }
      envs = envs.tail
    }
    return None
  }

  def bindEdges[T](f: (String,Edge) => Option[T]): Option[T] = {
    bindEnv({case (x:String, cs: ComplexShape) =>
      cs.shapes.find((ss => ss.edges.exists({case y:Edge => f(x,y).isDefined}))) match {
        case Some(ss) => ss.edges.find({case y:Edge => f(x,y).isDefined}).
          map(y => f(x,y.asInstanceOf[Edge]).get)
        case None =>
          bindEnv({case (x: String, ss: SimpleShape) =>
            ss.edges.find({case y:Edge => f(x,y).isDefined}).
              map(y => f(x,y.asInstanceOf[Edge]))
          case _ => None}) match {
            case Some(x) => x
            case None =>
              bindEnv({case (x: String, edg: Edge) => f(x,edg)
              case _ => None})}}
    case _ => None})
    }

  // @TODO: This is a major hack. The fundamental problem is that our notion of
  // "point value" does not maintain any sort of canonical name for an edge, which
  // on some deep level is probably going to mess up the correspondence between
  // complex shapes and simplicial sets.
  // @returns the simple shape containing the edges, list of other edges in its complex shape
  // @TODO: Find edges in space too
  def locateEdge(e: Edge): (Variable, SimpleShape, List[SimpleShape],RenamingSubstitution) = {
    // look for a complex shape containing the edge
    // if not found, look for a simple shape
    bindEnv({case (x:String, cs: ComplexShape) =>
      cs.shapes.find((ss => ss.edges.contains(e))) match {
        case None => None
        case Some(ss) => Some((Variable(x, None), ss, cs.shapes.filter(z => z != ss), cs.subst))
      }
    case _ => None}) match {
      case Some((w,x,y,z)) => (w,x,y,z)
      // @TODO: Search for standalone simple shape
      case None =>
        bindEnv({case (x: String, ss: SimpleShape) =>
        if (ss.edges.contains(e)) { Some((Variable(x, None), ss, Nil:List[SimpleShape], RenamingSubstitution.empty))}
        else None
      }) match {
          case Some(y) => y
          case _ =>
            bindEnv({ case (x: String, edg: Edge) =>
              if (edg == e) {
                Some((Variable(x, None), SimpleShape(List(e), Material(0, 0, 0)), Nil: List[SimpleShape], RenamingSubstitution.empty))
              }
              else None
            }).get
        }
    }
  }

  // @TODO: Hack for the same reasons
  def updateShapeInPlace(oldCS: Variable, newCS: AnyShape): State = {
    State(decls, env.map(e => e.map{case (k,v) =>
      if (k == oldCS.x) (k,newCS) else (k,v)
    }), space)
  }

  def nameOf(v: Value): Variable = {
    bindData({case (vr,v2) if v2 == v => Some(vr) case _ => None}) match {
      case Some(x) => x
      case None => ???
    }
  }

  def locateByEnd(e: Point): Edge = {
    bindEdges({case (x,edg) =>  if (edg.end == e) Some(edg) else None case _ => None}).get
  }

  def applySub(s: RenamingSubstitution): State = {
    val subMap: Map[Variable,Variable] = s.l.toMap
    val env2: List[Map[String,Value]] = env.map(m => m.map({case (k,v) =>
      if (subMap.contains(Variable(k,None))) (subMap(Variable(k,None)).x,v)
      else (k,v)
    }))
    State(decls,env2,space)
  }

  def addShapeToComplex(key: Variable, shape: SimpleShape): State = {
    val up =
      getVar(key) match {
        case ComplexShape(shs,sub) => ComplexShape(shape :: shs, sub)
        case sh: SimpleShape => ComplexShape(sh :: shape :: Nil, RenamingSubstitution.empty)
      }
    updateShapeInPlace(key, up)
  }
}
object State {
  val empty: State = State(Nil, Nil, Map())
}
