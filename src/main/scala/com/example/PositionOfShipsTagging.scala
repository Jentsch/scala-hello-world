package com.example

import shapeless.tag
import shapeless.tag.@@

import scala.collection.immutable.Iterable
import scala.math.{pow, sqrt}

class PositionOfShipsTagging(positions: Vector[Position]) {

  val positionsOf =
    positions.groupBy(_.ship)

  def allParticipatingShips: Iterable[Ship @@ WasParticipating] =
    positionsOf.collect { case (WasParticipating(s), _) => s }

  def avgSpeedOf(ship: Ship @@ WasParticipating): Double =
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

  trait WasParticipating

  object WasParticipating {
    def apply(s: Ship) = unapply(s)

    def unapply(s: Ship): Option[s.type @@ WasParticipating] =
      if (positionsOf(s).size >= 2)
        Some(tag[WasParticipating](s))
      else
        None
  }
}
