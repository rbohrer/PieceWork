import edu.wpi.rbohrer.piecework.{State,Variable,Point,Edge, ComplexShape,SimpleShape,Material,RenamingSubstitution}
import edu.wpi.rbohrer.piecework.penrose.PenroseConverter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec._
import org.scalatest.matchers.should.Matchers


class PenroseTests extends AnyFlatSpec with Matchers{
  val oneTri: State = State(Nil,
    List(Map(("myCS") -> ComplexShape(List(SimpleShape(List(
      Edge(Point(0.0,0.0),Point(0.0,1.0)),
      Edge(Point(0.0,1.0),Point(1.0,1.0)),
        Edge(Point(1.0,1.0),Point(0.0,0.0))
    ), Material(0,0,0))), RenamingSubstitution.empty))),
    Map()
  )

  "Penrose converter" should "translate one triangle" in {
    val res = PenroseConverter.substance(oneTri)
    println(res)
    val 2 = 1 + 1
  }
}
