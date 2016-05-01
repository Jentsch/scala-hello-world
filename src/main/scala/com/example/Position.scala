package com.example

import scala.math.abs

case class Position(time: Double, ship: Ship, position: Point) {
  def timeDelta(that: Position) =
    abs(this.time - that.time)
}
