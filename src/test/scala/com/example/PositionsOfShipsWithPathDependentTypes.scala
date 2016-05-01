package com.example

import org.scalatest._


class PositionsOfShipsWithPathDependentTypes extends FlatSpec with Matchers {

  val exampleData = new PositionOfShipsPath(Vector(
    Position(1, Luise, Point(100, 100)),
    Position(2, Luise, Point(100, 200)),
    Position(3, Luise, Point(100, 400)),
    Position(4, Luise, Point(200, 500)),
    Position(4, Berta, Point(100, 400))
  ))

  import exampleData._

  "positions of Luise" should "find at least two points" in {
    positionsOf(Luise).length should be >= 2
  }

  it should "not contain a position of Berta" in {
    no(positionsOf(Luise)) should matchPattern { case Position(_, Berta, _) => () }
  }

  "average speed of Luise" should "be greater than the minimal speed" in {
    WasParticipating(Luise).foreach { implicit l => // l ist nur der Beweis und muss nicht mehr direkt verwendet werden
      avgSpeedOf(Luise) should be > 100.0
    }
  }

  it should "be also smaller than the maximal speed" in {
    WasParticipating(Luise).foreach { implicit l =>
      avgSpeedOf(Luise) should be < 200.0
    }
  }

  it should "not throw an exception or return NaN of any participating ship" in {
    // das scheint nicht mehr so einfach möglich zu sein
    // die Zeile war nur mit brutalem Trickes möglich

    avgSpeedOf(allParticipatingShips.head)

    pending
    //    allParticipatingShips.foreach{ implicit s =>
    //      a[Exception] shouldNot be(thrownBy(avgSpeedOf(s)))
    //      avgSpeedOf(s).isNaN should be(false)
    //    }
  }

  it should "fail at compile time if no check was made for sufficient data points" in {
    "avgSpeedOf(Luise)" shouldNot compile
  }

  "Berta" should "not be participating" in {
    WasParticipating(Berta) should be(None)
  }
}
