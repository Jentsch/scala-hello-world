package com.example

import scala.math.{pow, sqrt}
import scala.util.Try


class PositionOfShips(positions: Vector[Position]) {

  // Diese Lösung wurde von fast allen gefunden:
  def positionsOf(ship: Ship): Vector[Position] =
    positions.filter(_.ship == ship)

  // Semantisch äquivalent wäre auch gewesen:
  val positionsOf2 =
    positions.groupBy(_.ship)
  // groupBy bildet eine Map[Ship, Vector[Position]], was eine Funktion ist von Ship => Vector[Position] ist.
  // Die Performace ist natürlich besser, allerdings benötigt die Hashmap zusätzlichen Speicher. (Klassischer Trade-off)
  // Die Signatur ist naturlich leicht anders, die Tests wären trotzdem grün geworden


  // Diese Lösung wurde heute beim Treffen entwickelt (sry, Namen vergessen)
  // Pro: sehr abstrakt, kann auf viele (auch parallel und verteilte) Datenstrukturen
  // Contra: sehr abstrakt, potentiell langsam
  def avgSpeedOf(ship: Ship): Double = {
    val pos: Vector[Position] =
      positionsOf(ship)
        .sortBy(_.time)

    val totalDistance =
      pos
        .sliding(2)
        .map {
        case Vector(a) => 0.0
        case Vector(a, b) =>
          a.position.distance(b.position)
      }
        .sum

    val totalTime = pos.head.timeDelta(pos.last)

    if (totalTime > 0.0) totalDistance / totalTime else 0.0
  }

  // Sehr ähnlich wie die Lösung oben, aber mehr mit den "Brot und Butter" Operationen
  // aus der funktionalen Programmierung
  // Pro und Contra wie oben
  def avgSpeedOfSimpleCombinators(ship: Ship): Double = {
    val pos: Vector[Position] =
      positionsOf(ship)
        .sortBy(_.time)

    val totalDistance =
      pos.zip(pos.tail)
        .foldLeft(0.0) { case (total, (a, b)) =>
        total + a.position.distance(b.position)
      }

    val totalTime = pos.head.timeDelta(pos.last)

    if (totalTime > 0.0) totalDistance / totalTime else 0.0
  }

  // Typischer Hammer in FP, wenn man nicht weiter weiß: Rekursion.
  // Pro: geringer Speicher und CPU verbrauch
  //     (insbesondere wenn eine Endrekursive Veriante verwendet werden)
  // Contra: funktioniert nur für diese Datenstuktur, nicht parallelsierbar
  def avgSpeedOfRecusive(ship: Ship): Double = {
    val pos: Vector[Position] =
      positionsOf(ship)
        .sortBy(_.time)

    def totalDistance(positions: Vector[Position]): Double =
      positions match {
        case a +: b +: rest => a.position.distance(b.position) + totalDistance(b +: rest)
        case _ => 0.0 // case _ => else
      }

    val totalTime = pos.head.timeDelta(pos.last)

    if (totalTime > 0.0) totalDistance(pos) / totalTime else 0.0
  }

  // Und zuletzt die eine imperative Variante
  // Pro: für die konkrete Datenstruktu am schnellsten
  // Contra: keine einfache Möglichkeit für Parallelisierung
  def avgSpeedOfImperative(ship: Ship): Double = {
    val pos: Vector[Position] =
      positionsOf(ship)
        .sortBy(_.time)

    var totalTime = 0.0
    var totalDistance = 0.0
    var previousPosition = pos(0)

    pos.foreach { currentPosition =>
      totalTime += currentPosition.timeDelta(previousPosition)
      totalDistance += currentPosition.position.distance(previousPosition.position)
      previousPosition = currentPosition
    }

    // Das Schlüsselwort "return" ist nicht notwendig, just for fun
    if (totalTime > 0.0)
      return totalDistance / totalTime
    else
      return 0.0
  }

  def knownShips: Vector[Ship] =
    positions.map(pos => pos.ship).distinct
}

case class Point(x: Double, y: Double) {
  // Diese oder eine ähnliche Lösung wurde von allen gefunden
  // Hier gibt es noch zwei kleine Veränderungen:
  //  - die Methoden pow und sqrt werden direkt import, somit ist es nicht notwendig math.sqrt zu schreiben
  //  - das Quadrieren ist nun eine eigene Methode
  //
  // Alternative Implementierungen die die Test grün werden ließen waren übgrigens:
  //  - def distance(that: Point) = 0.0
  //  - def distance(that: Point) = abs(this.x - that.x) + abs(this.y - that.y)
  def distance(that: Point): Double =
    sqrt(sqr(this.x - that.x) + sqr(this.y - that.y))

  private def sqr(x: Double) =
    pow(x, 2)
}

case class Position(time: Double, ship: Ship, position: Point) {
  def timeDelta(that: Position) =
    math.abs(this.time - that.time)
}
