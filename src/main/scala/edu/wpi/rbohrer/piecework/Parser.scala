
package edu.wpi.rbohrer.piecework

import fastparse.MultiLineWhitespace._
import fastparse.{P, _}

object Parser {
  def ident[_: P]: P[String] = {
    import fastparse.NoWhitespace._
    P (CharIn("a-z").rep(1).!)
  }

  def num[_: P]: P[Double] = {
    import fastparse.NoWhitespace._
    P( CharIn("0-9").rep(1).!.map(_.toDouble))
  }

  //def [_: P]: P[Expression] = {}

  def point[_: P]: P[Point] = {
    P("point"~ "(" ~ num ~ "," ~ num ~ ")").map({
      case (x,y) => Point(x,y)
    })
  }
  def edge[_: P]: P[Edge] = {
    P("edge" ~ "(" ~ point ~ "," ~ point ~ ")").map({
      case (x,y) => Edge(x,y)
    })
  }
  def wildcard[_: P]: P[Wildcard.type] = {
    P("_").map(_ => Wildcard)
  }

  def assignTuple[_: P]: P[AssignTuple] = {
    P("[" ~ assignable.rep(sep=",").map(_.toList) ~ "]").map(AssignTuple)
  }

  def assignable[_: P]: P[Assignable] = {
     wildcard | assignTuple | ident.map(x => AssignVar(Variable(x)))
  }

  def edgeList[_: P]: P[List[Edge]] = {
    P("[" ~ edge.rep(sep=",") ~ "]").map(_.toList)
  }

  def material[_: P]: P[Material] = {
    P("material" ~ "(" ~ num ~ "," ~ num ~ "," ~ num ~ ")").map({case (r,g,b) =>
    Material(r,g,b)})
  }
  def simpleShape[_: P]: P[SimpleShape] = {
    P("simpleShape" ~ "(" ~ edgeList ~ "," ~ material ~ ")").map({
      case (es, m) => SimpleShape(es,m)
    })
  }

  def simpleShapeList[_: P]: P[List[SimpleShape]] = {
    P(simpleShape.rep(sep=",").map(_.toList))
  }

  def renamingSubstitution[_: P]: P[RenamingSubstitution] = {
    P( "[" ~ ("(" ~ ident ~ "," ~ ident ~ ")").rep(sep=",") ~ "]").
      map(_.toList).map(RenamingSubstitution)
  }


  def complexShape[_: P]: P[ComplexShape] = {
    P("complexShape" ~ "(" ~  simpleShapeList ~ "," ~ renamingSubstitution ~ ")").map(
      {case (ssl, rs) => ComplexShape(ssl,rs)}
    )
  }

  def atomExpr[_: P]: P[Expression] = {
    point | edge | material | simpleShape | complexShape
  }

  def builtinMethod[_: P]: P[String] = {
    P("x" | "y" | "beg" | "end" | "edges" | "mat" | "shapes" | "subst").!
  }

  def numericFactor[_: P]: P[Expression] = {
    atomExpr.rep(sep="*",min=1).map(_.toList).map({
      case e :: Nil => e
      case ((e:Numeric) :: es) => es.foldLeft(e)({case (x:Numeric,y:Numeric) => Times(x,y)})
    })
  }

  def numericExpr[_: P]: P[Expression] = {
    (numericFactor ~ ((CharIn("+\\-").! ~ numericFactor)).rep.map(_.toList)).map({
      case (e, Nil )=> e
      case (e, es: List[(String,Expression)]) => es.foldLeft[Expression](e)({
         case (x:Numeric,("+", y: Numeric)) => Plus(x,y)
         case (x:Numeric,("-", y: Numeric)) => Minus(x,y)
      })
    })
  }

  def prop[_: P]: P[Proposition] = {
    (numericExpr ~ ("<=" | "<" | "=" | ">" | ">=").! ~ numericExpr).map({
      case (e1: Numeric ,"<=",e2: Numeric ) => LessEqual(e1,e2)
      case (e1: Numeric ,"<",e2: Numeric ) => Less(e1,e2)
      case (e1: Numeric ,"=",e2: Numeric ) => Equal(e1,e2)
      case (e1: Numeric ,">",e2: Numeric ) => Greater(e1,e2)
      case (e1: Numeric ,">=",e2: Numeric ) => GreaterEqual(e1,e2)

    })

  }

  def propExpr[_: P]: P[Expression] = prop | numericExpr

  def indexedExpr[_: P]: P[Expression] = {
    P(propExpr ~ "[" ~ numericExpr ~ "]").map({case (x,y: Numeric) => Indexed(x,y)})
  }

  def dottedExpr[_: P]: P[Expression] = {
    P ((indexedExpr ~ ("." ~ builtinMethod.rep(sep=".",min=0)).?).map({
      case (e, None) => e
      case (e, Some(kws)) =>
          kws.foldLeft(e)({
            case (e, "x") => DotX(e)
            case (e, "y") => DotY(e)
            case (e, "beg") => DotBeg(e)
            case (e, "end") => DotEnd(e)
            case (e, "edges") => DotEdges(e)
            case (e, "mat") => DotMat(e)
            case (e, "shapes") => DotShapes(e)
            case (e, "subst") => DotSubst(e)
          })}))}

  def expr[_: P]: P[Expression] = dottedExpr

  def markProg[_: P]: P[Mark] = P("mark" ~ "(" ~ edge ~ "," ~ numericExpr ~ ")").
    map({case(x,y:Numeric) => Mark(x,y)})

  def cutProg[_: P]: P[Cut] = P("mark" ~ "(" ~ expr ~ "," ~ expr ~ ")").
    map({case(x,y) => Cut(x,y)})

  def sewProg[_: P]: P[Sew] = P("sew" ~ "(" ~ expr ~ "," ~ expr ~ ")").
    map({case(x,y) => Sew(x,y)})

  def atomProg[_: P]: P[Program] = markProg | cutProg | sewProg

  def ifProg[_: P]: P[IfThenElse] = P("if" ~ "(" ~ expr ~ ")" ~ "{" ~  prog ~ "}" ~ "else" ~ "{" ~ prog ~  "}").
    map({case(x,y,z) => IfThenElse(x,y,z)})

  def whileProg[_: P]: P[While] = P("while" ~ "(" ~ expr ~ ")" ~ "{" ~  prog ~ "}").
    map({case(x,y) => While(x,y)})

  def returnProg[_: P]: P[Return] = P("return" ~ expr).map({case (x) => Return(x)})

  def callProg[_: P]: P[Call] = P(ident ~ ("(" ~ expr.rep(sep=",") ~ ")").map(_.toList)).map({case (id,args) =>
    Call(id,args)})

  def assignProg[_: P]: P[Assign] = P(assignable ~ ":=" ~ expr).map({case (x,y) => Assign(x,y)})

  def prefixProg[_: P]: P[Program] =
    atomProg  | ifProg | whileProg | returnProg | callProg

  def oneProg[_: P]: P[Program] = prefixProg | assignProg

  def prog[_: P]: P[Program] = oneProg.rep(sep=";").map(x => x.toList.reduceLeft[Program]({
    case (x,y) => Sequence(x,y)
  })) ~ ";".?


  def parseType[_: P]: P[Tp] = P(
    P("number").map(_ => NumericType)
  | P("point").map(_ => PointType)
  | P("edge").map(_ => EdgeType)
  | P("bool").map(_ => BoolType)
  | P("list" ~ "[" ~ parseType ~ "]").map(t => ListType(t)))

  def typedArgs[_: P]: P[List[(String,Tp)]] = P((parseType ~ ident).map({case(x,y)=>(y,x)}).rep(sep=",").map(_.toList))

  def funDecl[_: P]: P[FunDecl] =
    (P(ident ~ "(" ~ typedArgs ~ ")" ~ "{" ~ prog ~ "}")).map({ case (x,y,z) => FunDecl(x,y,z)})
  def decl[_: P]: P[Decl] = funDecl
  def file[_: P]: P[List[Decl]] = funDecl.rep.map(_.toList) ~ End
}
