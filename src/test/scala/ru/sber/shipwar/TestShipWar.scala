package ru.sber.shipwar

import org.scalatest.{FlatSpec, Matchers}
import ru.sber.shipwar.Naval.{Field, Fleet, Ship}

import scala.io.StdIn.readInt

class TestShipWar extends FlatSpec with Matchers {
	"A ship" should "should be validate ship war rules" in {
		val ships = List(
			List(),
			List((1, 12)),
			List((1, 2), (1, 4)),
			List((1, 2), (2, 3)),
			List((1, 1), (1, 2), (1, 3), (1, 4), (1, 5)),
			List((1, 1)),
			List((2, 2), (3, 2)),
			List((1, 1), (1, 2), (1, 3), (1, 4))
		)
		val expected = List(false, false, false, false, false, true, true, true)

		ships.indices.foreach{x =>
			val actual = ShipWar.validateShip(ships(x))
			assert(actual === expected(x))
		}
	}

	it should "Check size after trying ship to fleet" in {
		val game: (Field, Fleet) = (Lesson.field, Map.empty[String, Ship])
		val expected = 3
		val fleet = Map{
			"Q1" -> List((1,1))
			"Q2" -> List((1,2), (1,3))
			"Q3" -> List((3,5), (3,6), (3,7))
			"Q4" -> List((8,8), (9,8), (10,8), (11, 8))
			"Q5" -> List((9,1))
		}
		println(fleet)
		assert(ShipWar.output(game, fleet.toList)._2.size === expected)
	}
}