package de.htwg.kakuro.controller

import de.htwg.kakuro.model.PlayField
import scala.swing.event.Event
import scala.swing.Publisher

import _root_.de.htwg.kakuro.model.Cell
import _root_.de.htwg.kakuro.model.SumCell

case class ChangeCell extends Event
case class NewPlayField extends Event
case class CheckCell(result: String) extends Event

class KakuroController(var model: PlayField) extends Publisher {

	var statusInfo = "Start game"
	
	def reset = {
		model.reset
		publish(new ChangeCell)
	}

	def check = {
		model.check.foreach(arg => publish(new CheckCell(arg)))
	}

	def load(name: String) = {
		model.load(name)
		publish(new NewPlayField)
	}
	
	def setValue(row: Int, column: Int, value: Int){
		try {
			model.cells(row)(column) match {
			case c: Cell => c <== value
			case c: SumCell => println("False cell.")
			}
		} catch {
			case e: scala.MatchError => println("False cell.")
			case e: java.lang.ArrayIndexOutOfBoundsException => println("Unknown cell.")
		}
		publish(new ChangeCell)
	}
	
	def getCell(row: Int, column: Int){
		try {
			model.cells(row)(column) match {
			case c: Cell => println(c.celltoString + "\n")
			case c: SumCell => println("False cell.")
			}
		} catch {
			case e: scala.MatchError => println("False cell.")
			case e: java.lang.ArrayIndexOutOfBoundsException => println("Unknown cell.")
		}
	}
}