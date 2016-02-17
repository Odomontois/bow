package bow

import scalaz.syntax.arrow._

/**
  * User: Oleg
  * Date: 17-Feb-16
  * Time: 22:01
  */
sealed trait Flow[=>:[_, _], I, O] {

}

case class Input[=>:[_, _], I, O](run: I =>: Flow[=>:, I, O]) extends Flow[=>:, I, O]

case class Elem[=>:[_, _], I, O](value: O, next: Flow[=>:, I, O]) extends Flow[=>:, I, O]

case class Skip[=>:[_, _], I, O](run: Unit =>: Flow[=>:, I, O]) extends Flow[=>:, I, O]

case class End[=>:[_, _], I, O]() extends Flow[=>:, I, O]

object Flow {
  def runStream[=>:[_, _], I, O](flow: Flow[=>:, I, O])(implicit A: ArrowApply[=>:]): Stream[I] =>: Stream[O] = flow match {
    case End() => A.arr(_ => Stream.empty)
    case Elem(value, next) => runStream(next) >>^ (value #:: _)
    case Skip(run) => A.unwrap(run >>^ { x => runStream(x) })
    case Input(run) => ???
  }
}


