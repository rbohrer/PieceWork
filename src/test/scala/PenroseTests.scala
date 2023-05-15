import edu.wpi.rbohrer.piecework.{ComplexShape, Edge, Interpreter, Material, Parser, Point, RenamingSubstitution, SimpleShape, State, Variable}
import edu.wpi.rbohrer.piecework.penrose.PenroseConverter
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers


class PenroseTests extends AnyFlatSpec with Matchers{

  val sierpinski: String =
    """divideTriangle(shape t, number i) {
      |if (i > 0) {
      |[te1, te2, te3] := t.edges;
      |new1 := mark(te1, 0.5);
      |new2 := mark(te2, 0.5);
      |new3 := mark (te3, 0.5);
      |[_, t1] := cut(new1, new2);
      |[_, t2] := cut(new3, new2);
      |[t4, t3] := cut(new3, new1);
      |div1 := divideTriangle(t1, i-1);
      |div2 := divideTriangle(t2, i-1);
      |div3 := divideTriangle(t3, i-1);
      |div4 := divideTriangle(t4, i-1);
      |return complexShape([div1,div2,div3,div4], []);
      |} else {
      |return t;
      |}
      |}
      |main(){p1 := point(0,0);
      |p2 := point(2,0);
      |p3 := point(1,1.732);
      |e1 := edge(p1,p2);
      |e2 := edge(p2,p3);
      |e3 := edge(p3,p1);
      |red := material(255,0,0);
      |tri := shape([e1, e2, e3], red);
      |stri := divideTriangle(tri, 1);
      |}""".stripMargin

  val chaikin: String =
    """chaikin(shape s, number i) {
      |if (i > 0) {
      |l := s.edges.length;
      |j := l-1;
      |while(j >= 0) {
      |e1 := s.edges[j];
      |if (j = l - 1){
      |  e2 := s.edges[0];
      |  p1 := mark(e1, 0.75);
      |  p2 := mark(e2, 0.25);
      |  [_, s] := cut(p1, p2);
      |} else {
      |  e2 := s.edges[j+1];
      |  p1 := mark(e1, 0.75);
      |  p2 := mark(e2, 0.25);
      |  [s, _] := cut(p1, p2);
      |
      |}
      |j := j - 1;
      |}
      |return chaikin(s, i-1);
      |} else {
      |return s;
      |}
      |}
      |main(){
      |p1 := point(0,0);
      |p2 := point(0,1);
      |p3 := point(1,1);
      |p4 := point(1,0);
      |e1 := edge(p1,p2);
      |e2 := edge(p2,p3);
      |e3 := edge(p3,p4);
      |e4 := edge(p4, p1);
      |blue := material(0,0,255);
      |square := shape([e1,e2,e3,e4], blue);
      |chaikin(square, 1);
      |}
      |""".stripMargin


  val oneTri: State = State(Nil,
    List(Map(("myCS") -> ComplexShape(List(SimpleShape(List(
      Edge(Point(0.0,0.0),Point(0.0,1.0)),
      Edge(Point(0.0,1.0),Point(1.0,1.0)),
        Edge(Point(1.0,1.0),Point(0.0,0.0))
    ), Material(0,0,0))), RenamingSubstitution.empty))),
    Map()
  )

   /*should "translate one triangle" in {
    val res = PenroseConverter.substance(oneTri)
    println(res)
    val 2 = 1 + 1
  }

  it */ "Penrose converter" should "translate the Sierpinski example" in {
    val x = fastparse.parse(sierpinski, Parser.file(_))
    val Parsed.Success(file,i) = x
    val (_, pwprog) = Interpreter(file)
    val penroseprog = PenroseConverter.substance(pwprog)
    println(penroseprog)
    val 2 = 1 + 1

  }
  it should "translate the Chaikin example" in {
    val x = fastparse.parse(chaikin, Parser.file(_))
    val Parsed.Success(file,i) = x
    val (_, pwprog) = Interpreter(file)
    val penroseprog = PenroseConverter.substance(pwprog)
    println(penroseprog)
    val 2 = 1 + 1
  }

  val sierpOutput:String = """Vertex x_3
                             |Vertex x_1
                             |Vertex x_0
                             |Vertex x_2
                             |Vertex x_4
                             |Vertex x_5
                             |Edge e_10 := MakeEdge(x_0,x_4)
                             |Edge e_11 := MakeEdge(x_4,x_5)
                             |Edge e_6 := MakeEdge(x_0,x_2)
                             |Edge e_2 := MakeEdge(x_2,x_0)
                             |Edge e_1 := MakeEdge(x_1,x_2)
                             |Edge e_9 := MakeEdge(x_5,x_0)
                             |Edge e_8 := MakeEdge(x_4,x_0)
                             |Edge e_7 := MakeEdge(x_2,x_4)
                             |Edge e_5 := MakeEdge(x_4,x_2)
                             |Edge e_3 := MakeEdge(x_2,x_3)
                             |Edge e_0 := MakeEdge(x_0,x_1)
                             |Edge e_4 := MakeEdge(x_3,x_4)
                             |Triangle stri_0 := MakeTriangle(x_0,x_1,x_2)
                             |Triangle stri_1 := MakeTriangle(x_2,x_3,x_4)
                             |Triangle stri_2 := MakeTriangle(x_0,x_2,x_4)
                             |Triangle stri_3 := MakeTriangle(x_5,x_0,x_4)
                             |Label stri_0 "stri"
                             |""".stripMargin
  val chaikinOutput: String = """Vertex x_9
                        |Vertex x_1
                        |Vertex x_4
                        |Vertex x_11
                        |Vertex x_10
                        |Vertex x_7
                        |Vertex x_0
                        |Vertex x_8
                        |Vertex x_2
                        |Vertex x_3
                        |Vertex x_6
                        |Vertex x_5
                        |Edge e_7 := MakeEdge(x_7,x_0)
                        |Edge e_4 := MakeEdge(x_4,x_5)
                        |Edge e_1 := MakeEdge(x_1,x_2)
                        |Edge e_6 := MakeEdge(x_6,x_7)
                        |Edge e_8 := MakeEdge(x_8,x_9)
                        |Edge e_11 := MakeEdge(x_11,x_8)
                        |Edge e_5 := MakeEdge(x_5,x_6)
                        |Edge e_10 := MakeEdge(x_10,x_11)
                        |Edge e_0 := MakeEdge(x_0,x_1)
                        |Edge e_3 := MakeEdge(x_3,x_4)
                        |Edge e_2 := MakeEdge(x_2,x_3)
                        |Edge e_9 := MakeEdge(x_9,x_10)
                        |Triangle s_5 := MakeTriangle(x_0,x_6,x_7)
                        |Triangle s_4 := MakeTriangle(x_0,x_5,x_6)
                        |Triangle s_3 := MakeTriangle(x_0,x_4,x_5)
                        |Triangle s_2 := MakeTriangle(x_0,x_3,x_4)
                        |Triangle s_1 := MakeTriangle(x_0,x_2,x_3)
                        |Triangle s := MakeTriangle(x_0,x_1,x_2)
                        |Triangle square_1 := MakeTriangle(x_8,x_10,x_11)
                        |Triangle square := MakeTriangle(x_8,x_9,x_10)
                        |""".stripMargin
}
