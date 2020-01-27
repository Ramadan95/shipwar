import org.scalatest.{FlatSpec, Matchers}
import ru.sber.shipwar.{Lesson, ShipWar}
import ru.sber.shipwar.Naval.{Field, Fleet, Ship}

import scala.io.StdIn.readInt

class TestShipWar extends FlatSpec with Matchers {
	def testValidateShip() = {
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

		for (i <- ships.indices) {
			println(s"Now testing ship: ${ships(i)}")
			val actual = ShipWar.validateShip(ships(i))
			actual == expected(i) match {
				case true => println(s"Passed!\texpected: ${expected(i)}\tactual: $actual\n")
				case false => println(s"FAILED!\texpected: ${expected(i)}\tactual: $actual\n")
			}
		}
	}

	testValidateShip()

	def testAddAndTryAddShip(): Unit = {
		val game: (Field, Fleet) = (Lesson.field, Map.empty[String, Ship])
		val expected = 2

		println(ShipWar.output(game, ShipWar.input(Map.empty[String, Ship],
			readInt).toList)._2.size == expected)
		//Copy and Paste in ConsoleWrite
		//4
		//Q1 2
		//1 1
		//1 2
		//Q2 2
		//3 3
		//3 4
		//Q3 4
		//10 10
		//9 10
		//8 10
		//7 10
		//Q4 2
		//5 1
		//5 0
	}
	testAddAndTryAddShip()
}