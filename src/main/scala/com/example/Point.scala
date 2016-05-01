package com.example

import scala.math._

case class Point(x: Double, y: Double) {
  def distance(that: Point): Double =
    sqrt(sqr(this.x - that.x) + sqr(this.y - that.y))

  private def sqr(x: Double) =
    pow(x, 2)
}

