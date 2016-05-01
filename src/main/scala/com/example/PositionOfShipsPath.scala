package com.example

class PositionOfShipsPath(positions: Vector[Position]) {

  val positionsOf =
    positions.groupBy(_.ship)
  val allParticipatingShips: ::[Ship] =
    positionsOf.collect { case (s@WasParticipating(_), _) => s }.toList.asInstanceOf[::[Ship]]
  implicit val headOfAll = new WasParticipating[allParticipatingShips.head.type] {}

  def avgSpeedOf(ship: Ship)(implicit wasParticipating: WasParticipating[ship.type]): Double =
    unsafeAvgSpeed(ship).get

  private def unsafeAvgSpeed(ship: Ship): Option[Double] = {
    val pos =
      positionsOf(ship)
        .sortBy(_.time)
        .view

    val totalDistance =
      pos
        .zip(pos.tail)
        .map { case (a, b) =>
          a.position.distance(b.position)
        }
        .sum


    val timeDelta = pos.head.timeDelta(pos.last)

    if (timeDelta != 0)
      Some(totalDistance / timeDelta)
    else
      None
  }

  def knownShips: Set[Ship] =
    positions.map(pos => pos.ship).toSet

  trait WasParticipating[S <: Ship]

  object WasParticipating {
    def apply(s: Ship) = unapply(s)

    def unapply(s: Ship): Option[WasParticipating[s.type]] =
      if (positionsOf(s).size >= 2)
        Some(new WasParticipating[s.type] {})
      else
        None
  }
}
