package de.htwg.kakuro.view.tui

import de.htwg.kakuro.controller.KakuroController
import de.htwg.kakuro.controller.ChangeCell
import de.htwg.kakuro.controller.NewPlayField
import de.htwg.kakuro.controller.CheckCell
import scala.swing._
import de.htwg.kakuro.model.PlayField

class Tui(var controller: KakuroController) extends Reactor {

	listenTo(controller)

	update

	reactions += {
		case e: ChangeCell => update
		case e: NewPlayField => update
		case e: CheckCell => println(e.result)
	}

	def update = {
		println("=============================================================")
		println(controller.model.toString)
		println("\nEnter command:")
		println("\nrow coulumn value = set value")
		println("row coulumn = schow value\n")
		println("n = New play field.")
		println("c = Check play field.")
		println("r = Reset play field.")
		println("l = Load a play field.")
		println("q = Quit.")
	}

	def display(commando: String) = {

		var run = true
		commando match {
			case "n" =>
				//				println("Please file name:")
				//				var filename = readLine
				controller.load("data/easy.ini")
			case "c" => controller.check
			case "r" => controller.reset
			case i: String if (i.split(" ").toList.length == 3) =>
				i.split(" ").toList.filter(c => c != ' ').map(c => c.toString().toInt) match {
					case row :: column :: value :: Nil => controller.setValue(row, column, value)
					case _ =>
				}
			case i: String if (i.split(" ").toList.length == 2) =>
				i.split(" ").toList.filter(c => c != ' ').map(c => c.toString().toInt) match {
					case row :: column :: Nil => controller.getCell(row, column)
					case _ =>
				}
			case "l" =>
				println("Please enter the filename, e.g. data/default.ini:")
				var name = readLine
				controller.load("data/" + name)
			case "q" => run = false
			case _ => printf("Command not found\n\n")
		}
		run
	}
}