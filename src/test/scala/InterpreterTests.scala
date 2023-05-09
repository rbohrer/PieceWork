import edu.wpi.rbohrer.piecework.Interpreter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterTests extends AnyFlatSpec with Matchers {
  import edu.wpi.rbohrer.piecework.Parser
  import fastparse.Parsed
  import org.scalatest.flatspec._
  import org.scalatest.matchers.should._

  val sierpinski: String =
    """divideTriangle(shape t, number i) {
      |if (i > 0) {
      |new1 := mark(t.edges[0], 0.5);
      |new2 := mark(t.edges[1], 0.5);
      |new3 := mark (t.edges[2], 0.5);
      |[t1, t6] := cut(new1, new2);
      |[t3, t5] := cut(new3, new2);
      |[t4, t2] := cut(new3, new1);
      |div1 := divideTriangle(t1, i-1);
      |div2 := divideTriangle(t2, i-1);
      |div3 := divideTriangle(t3, i-1);
      |div4 := divideTriangle(t4, i-1);
      |j := 0;
      |while (j < i) {
      |sew(div2.edges[j], div1.edges[j]);
      |sew(div2.shapes[0].edges[i + j], div3.edges[i + j]);
      |sew(div2.shapes[0].edges[(2 * i) + j], div4.edges[(2 * i) + j]);
      |j := j + 1;
      |}
      |return div2;
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

  "Parse+Interpret" should "run the Chaikin example" in {
    val x = fastparse.parse(chaikin, Parser.file(_))
    val Parsed.Success(file,i) = x
    val res = Interpreter(file)
    println(res)
  }

  it should "run the Sierpinski example" in {
    val x = fastparse.parse(sierpinski, Parser.file(_))
    val Parsed.Success(file,i) = x
    val res = Interpreter(file)
    println(res)
  }
}
