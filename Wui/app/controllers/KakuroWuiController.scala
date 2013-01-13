package controllers

import de.htwg.kakuro
import de.htwg.kakuro.view
import de.htwg.kakuro.main.Start
import java.awt.TextField
import _root_.scala.swing._
import de.htwg.kakuro.model.Cell
import de.htwg.kakuro.model.SumCell
import de.htwg.kakuro.view.gui.Gui
import de.htwg.kakuro.controller.ChangeCell
import de.htwg.kakuro.controller.NewPlayField
import de.htwg.kakuro.controller.CheckCellsResult
import de.htwg.kakuro.controller.CheckCell



class KakuroWuiController { 
	val start = Start

	def check = start.controller.check
	def reset = start.controller.reset
	def level(name: String) = start.controller.load(name)
  	def setValue(row: Int, column: Int, value:Int) = Start.controller.setValue(row.toInt, column.toInt, value.toInt)
}