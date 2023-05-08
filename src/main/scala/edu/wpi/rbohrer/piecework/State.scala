package edu.wpi.rbohrer.piecework

case class State(var decls: List[Decl], var env: List[Map[String,Value]]) {
  def addDecls(ds: List[Decl]): State = State(decls ++ ds, env)
  def updateVar(x: String, v: Value): State = {
    val (e :: es) = env
    val e1 = e.+(x -> v)
    State(decls, e1 :: es)
  }

  def getVar(x: String): Value  = {
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

  def main: FunDecl = decls.last.asInstanceOf[FunDecl]
  def call(xs: List[String], vs: List[Value]): State = {
    val argEnv = xs.zip(vs).toMap
    State(decls, argEnv::env)
  }
  def ret: State = {
    State(decls, env.tail)
  }

  def findFunction(name: String): FunDecl = {
    decls.find({case fd : FunDecl if fd.name == name => true}).get.asInstanceOf[FunDecl]
  }

  def bindEnv[T](f: (String,Value) => Option[T]): Option[T] = {
    var envs = env
    while(envs.nonEmpty) {
      var thisEnv = envs.head.toList
      while(thisEnv.nonEmpty)
      {
        val (k,v) = thisEnv.head
        f(k,v) match {
          case Some(z) => return Some(z)
          case _ => thisEnv = thisEnv.tail
        }
      }
      envs = env.tail
    }
    return None
  }

  // @TODO: This is a major hack. The fundamental problem is that our notion of
  // "point value" does not maintain any sort of canonical name for an edge, which
  // on some deep level is probably going to mess up the correspondence between
  // complex shapes and simplicial sets.
  // @returns the simple shape containing the edges, list of other edges in its complex shape
  def locateEdge(e: Edge): (Variable, SimpleShape, List[SimpleShape],RenamingSubstitution) = {
    // look for a complex shape containing the edge
    // if not found, look for a simple shape
    bindEnv({case (x:String, cs: ComplexShape) =>
      cs.shapes.find((ss => ss.edges.contains(e))) match {
        case None => None
        case Some(ss) => Some((Variable(x), ss, cs.shapes.filter(z => z != ss), cs.subst))
      }
    case _ => None}) match {
      case Some((w,x,y,z)) => (w,x,y,z)
      // @TODO: Search for standalone simple shape
      case None =>
        bindEnv({case (x: String, ss: SimpleShape) =>
        if (ss.edges.contains(e)) { Some((Variable(x), ss, Nil:List[SimpleShape], RenamingSubstitution.empty))}
        else None
      }).get
    }
  }

  // @TODO: Hack for the same reasons
  def updateShapeInPlace(oldCS: Variable, newCS: AnyShape): State = {
    State(decls, env.map(e => e.map{case (k,v) =>
      if (k == oldCS.x) (k,newCS) else (k,v)
    }))
  }

  def nameOf(v: Value): String = {
    bindEnv({case (k,v2) if v2 == v => Some(k) case _ => None}).get
  }

  def locateByEnd(e: Point): Edge = {
    bindEnv({case (k,edg:Edge) if edg.end == e => Some(edg) case _ => None}).get
  }

  def applySub(s: RenamingSubstitution): State = {
    val subMap: Map[String,String] = s.l.toMap
    val env2: List[Map[String,Value]] = env.map(m => m.map({case (k,v) =>
      if (subMap.contains(k)) (subMap(k),v)
      else (k,v)
    }))
    State(decls,env2)
  }

  def addShapeToComplex(key: Variable, shape: SimpleShape): State = {
    val up =
      getVar(key.x) match {
        case ComplexShape(shs,sub) => ComplexShape(shape :: shs, sub)
        case sh: SimpleShape => ComplexShape(sh :: shape :: Nil, RenamingSubstitution.empty)
      }
    updateShapeInPlace(key, up)
  }
}
object State {
  val empty: State = State(Nil, Nil)
}
