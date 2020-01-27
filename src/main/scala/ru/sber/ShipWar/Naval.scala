package ru.sber.ShipWar

object Naval {
	type Point = (Int, Int)
	type Field = Vector[Vector[Boolean]]
	type Ship = List[Point]
	type Fleet = Map[String, Ship]
	type Game = (Field, Fleet)
}