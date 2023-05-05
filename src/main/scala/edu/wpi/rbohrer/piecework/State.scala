package edu.wpi.rbohrer.piecework

case class State(var decls: List[Decl], var env: Map[String,Value] ) {
  def addDecls(ds: List[Decl]): State = State(decls ++ ds, env)
  def updateVar(x: String, v: Value): State =
    State(decls, env.+(x -> v))
  def getVar(x: String): Value  = env(x)
  def main: FunDecl = decls.last.asInstanceOf[FunDecl]
}
object State {
  val empty: State = State(Nil, Map())
}
