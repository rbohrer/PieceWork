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
      |new1 := mark(t.edges[0], 0.5);
      |new2 := mark(t.edges[1], 0.5);
      |new3 := mark (t.edges[2], 0.5);
      |[t1, t2] := cut(new1, new2);
      |[t3, t2] := cut(new3, new2);
      |[t4, t2] := cut(new3, new1);
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
      |j := 0;
      |while(j < s.edges.length) {
      |e1 := s.edges[j];
      |e2 := s.edges[j+1];
      |l := s.edges.length
      |if (j = l - 1){
      |e2 := s.edges[0];
      |}
      |p1 := mark(e1, 0.75);
      |p2 := mark(e2, 0.25);
      |[_, s] := cut(p1, p2);
      |}
      |chaikin(s, i-1);
      |} else {
      |return s;
      |}
      |}
      |main(){
      |p1 := point(0,0);
      |p2 := point(0,1);
      |p3 := point(1,1);
      |p4 := point(0,1);
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
}
