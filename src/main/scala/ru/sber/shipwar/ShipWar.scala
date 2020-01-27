package ru.sber.shipwar

import Naval.{Field, Fleet, Game, Ship}
import scala.io.StdIn._

object ShipWar {
	def main(args: Array[String]): Unit = {
		print(output((Lesson.field, Map.empty[String, Ship]), input(Map.empty[String, Ship],
			readInt).toList)._2.keys.mkString("\n")) // Вывести названия кораблей, которые проходят по условия
	}
	val valPos : (Field, (Int, Int)) => Vector[Vector[Boolean]] = (field, ship) => field
		.slice(ship._1 - 2, ship._1 + 1)
		.map(x => x slice(ship._2 - 2, ship._2 + 1))
	val bord : (Int, Int) => Boolean = (point1, point2) =>
	{(point1 > 0 && point1 <= Lesson.field.length) && (point2 > 0 && point2 <= Lesson.field.length)}
	def bordEq(list : List[(Int, Int)]): Boolean = list.length == list.count(x => bord(x._1, x._2))
	def partEq : List[Int] => Boolean = list => list.distinct.length == 1
	def parUnEq : List[Int] => Boolean = list =>
		list.zipWithIndex.map(x => x._1 - x._2).distinct.length == 1 ||
			list.zipWithIndex.map(x => x._1 + x._2).distinct.length == 1
	def validateShip(ship: Ship): Boolean = {
		val first = ship.map(x => x._1)
		val second = ship.map(x => x._2)
		if (ship.nonEmpty && ship.length <= 4 &&  bordEq(ship)) {
			if ((partEq(first) && parUnEq(second)) || (partEq(second) && parUnEq(first))) true else false
		} else false
	} // определить, подходит ли корабль по своим характеристикам

	@scala.annotation.tailrec
	def validatePosition(ship: Ship, field: Field): Boolean = {
		ship match {
			case ::(head, next) =>
				if (valPos(field, head).distinct == Vector(Vector(false, false))
					|| valPos(field, head).distinct == Vector(Vector(false, false, false)))
					validatePosition(next, field)
				else false
			case Nil 			=> true
		}
	} // определить, можно ли его поставить

	def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
		fleet + (name -> ship)
	} // добавить корабль во флот
	def markUsedCells(field: Field, ship: Ship): Field = {
		ship match {
			case ::(head, next) =>
				markUsedCells(field.zipWithIndex
					.map(y => y._1.zipWithIndex
						.map(x => if (((x._2 > head._2 - 3 && x._2 < head._2 + 1) || x._1) &&
							(y._2 > head._1 - 3 && y._2 < head._1 + 1)) true
						else false)),
					next)
			case Nil => field
		}
	} // пометить клетки, которые занимает добавляемый корабль

	def tryAddShip(game: Game, name: String, ship: Ship): Game = {
		if (validateShip(ship) && validatePosition(ship, game._1)) {
			(markUsedCells(game._1, ship), game._2 ++ enrichFleet(game._2, name, ship))
		} else (game._1, game._2)
	} // логика вызовов методов выше

	def input(fleet: Fleet, count:Int): Fleet = {
		def addShip(ship: Ship, shipLevel: Int): Ship = {
			if (shipLevel == 0) ship
			else {
				val points = readLine().split(" ").map(_.toInt)
				if (ship.isEmpty) addShip(List((points(0), points(1))), shipLevel - 1)
				else addShip(ship :+ (points(0), points(1)), shipLevel - 1)
			}
		}
		if (count == 0) fleet else {
			val name_level = readLine().split(" ")
			input(fleet ++ Map(name_level(0) -> addShip(List.empty[(Int, Int)], name_level(1).toInt)), count - 1)
		}
	} // Вернуть Map входных данных

	def output(game: Game, fleet: List[(String, Ship)]): Game = {
		fleet match {
			case ::(head, next) => output(tryAddShip(game, head._1, head._2), next)
			case Nil => game
		}
	} // добавить в Game корабли, которые проходят по условиям

}