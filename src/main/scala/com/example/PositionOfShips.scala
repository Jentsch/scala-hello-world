package com.example


class PositionOfShips(positions: Vector[Position]) {

  def positionsOf(ship: Ship): Vector[Position] =
    ???

  def avgSpeedOf(ship: Ship): Double =
    ???

  def knownShips: Vector[Ship] =
    positions.map(pos => pos.ship).distinct
}

case class Point(x: Double, y: Double) {
  def distance(that: Point): Double =
    ???
}

case class Position(time: Double, ship: Ship, position: Point) {
  def timeDelta(that: Position) =
    math.abs(this.time - that.time)
}
