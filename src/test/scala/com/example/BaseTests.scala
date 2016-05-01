package com.example

import org.scalatest.{FlatSpec, Matchers}

class BaseTests extends FlatSpec with Matchers {

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
}
