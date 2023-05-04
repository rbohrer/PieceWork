import edu.wpi.rbohrer.piecework.Parser
import fastparse.Parsed
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

class ParserTests extends AnyFlatSpec with Matchers{


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


  "expression parser" should "parse number" in {
    val input = "0"
    val x = fastparse.parse(input, Parser.expr(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
  it should "parse variable" in {
    val input = "t"
    val x = fastparse.parse(input, Parser.ident(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
  it should "parse index and access" in {
    val input = "t.edges[0]"
    val x = fastparse.parse(input, Parser.expr(_))
    print(x)
    val Parsed.Success(_,_) = x

  }
  "oneprog parser" should "parse one mark" in {
    val input = "mark(t.edges[0], 0.5)"
    val x = fastparse.parse(input, Parser.oneProg(_))
    print(x)
    val Parsed.Success(_,_) = x
  }

  it should "parse one prog" in {
    val input = "new1 := mark(t.edges[0], 0.5)"
    val x = fastparse.parse(input, Parser.oneProg(_))
    print(x)
    val Parsed.Success(_,_) = x
  }

  "seqprog parser" should "parse example" in {
    val input =
    """new1 := mark(t.edges[0], 0.5);
      |new2 := mark(t.edges[1], 0.5);
      |new3 := mark (t.edges[2], 0.5);
      |[t1, t2] := cut(new1, new2);
      |[t3, t2] := cut(new3, new2);
      |[t4, t2] := cut(new3, new1);
      |div1 := divideTriangle(t1, i-1);
      |div2 := divideTriangle(t2, i-1);
      |div3 := divideTriangle(t3, i-1);
      |div4 := divideTriangle(t4, i-1);
      |""".stripMargin
//    val lines = input.linesIterator.toList
    //val x = lines.map({case l => fastparse.parse(l, Parser.prog(_))})
    val x = fastparse.parse(input, Parser.prog(_))
    print(x)
    val Parsed.Success(_,_) = x//(0)

  }
  it should "parse recursive call" in {
    val input = "divideTriangle(t1, i-1)"
    val x = fastparse.parse(input, Parser.prog(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
  it should "allow nested field accesses" in {
    val input = "sew(div2.shapes[0].edges[i + j], div3.edges[i + j])"
    val x = fastparse.parse(input, Parser.prog(_))
    print(x)
    val Parsed.Success(_, _) = x
  }

  it should "parse sierp body" in {
    val input = """if (i > 0) {
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
                  |}""".stripMargin
    val x = fastparse.parse(input, Parser.prog(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
  it should "parse sierp decl" in {
    val input = """divideTriangle(shape t, number i) {
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
                  |}""".stripMargin
    val x = fastparse.parse(input, Parser.file(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
  it should "parse sierp main" in {
    val input = """main(){
                  |p1 := point(0,0);
                  |p2 := point(2,0);
                  |p3 := point(1,1.732);
                  |e1 := edge(p1,p2);
                  |e2 := edge(p2,p3);
                  |e3 := edge(p3,p1);
                  |red := material(255,0,0);
                  |tri := shape([e1, e2, e3], red);
                  |stri := divideTriangle(tri, 1);
                  |}""".stripMargin
    val y = fastparse.parse("stri := divideTriangle(tri, 1);", Parser.oneProg(_))

    val x = fastparse.parse(input, Parser.file(_))
    print(x)
    val Parsed.Success(_,_) = x
  }

  "arglist parser" should "parse arglist example" in {
    val x = fastparse.parse("shape t, number i", Parser.typedArgs(_))
    print(x)
    val Parsed.Success(_,_) = x
  }

  "file parser" should "parse the Sierpinski example" in {
    val x = fastparse.parse(sierpinski, Parser.file(_))
    print(x)
    val Parsed.Success(_,_) = x
  }

  it should "parse the Chaikin example" in {
    val x = fastparse.parse(chaikin, Parser.file(_))
    print(x)
    val Parsed.Success(_,_) = x
  }
}
