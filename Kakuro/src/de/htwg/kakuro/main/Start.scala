package de.htwg.kakuro.main

import de.htwg.kakuro.model.PlayField
import de.htwg.kakuro.controller.KakuroController
import de.htwg.kakuro.view.tui.Tui
import de.htwg.kakuro.view.gui.Gui

object Start {
	
	def main(args: Array[String]){
		
		val model = new PlayField
		val controller = new KakuroController(model)
		val tui = new Tui(controller)
		val gui = new Gui(controller)

		while(tui.display(readLine())){}
	}
}