package com.example

import org.scalatest._


class PositionsOfShips extends FlatSpec with Matchers {

  "two distance between two points" should "be 0 if the the given points are equal" in {
    val a = Point(1, 2)
    val b = Point(1, 2)

    a.distance(b) should be(0.0)
  }

  it should "be symmetric" in {
    val a = Point(1, 2)
    val b = Point(2, 1)

    a.distance(b) should be(b.distance(a))
  }

  it should "lower than the distance to a third point" in {
    val a = Point(1, 2)
    val b = Point(2, 1)
    val c = Point(2, 2)

    a.distance(b) should be <= (a.distance(c) + c.distance(b))
  }

  val exampleData = new PositionOfShips(Vector(
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
    avgSpeedOf(Luise) should be > 100.0
  }

  it should "be also smaller than the maximal speed" in {
    avgSpeedOf(Luise) should be < 200.0
  }
}
