package de.htwg.kakuro.view.gui

import scala.swing.MainFrame
import java.awt.Dimension
import scala.swing.BorderPanel
import scala.swing.ListView
import scala._
import scala.swing.Button
import scala.swing.Action
import java.awt.Font
import scala.swing.ScrollPane

class Score(items: List[String]) extends MainFrame {
	title = "Score"
	preferredSize = new Dimension(320, 240)

	val editScroll = new ScrollPane {
		contents = new ListView(items) {
			foreground = java.awt.Color.WHITE
			background = java.awt.Color.BLACK
			font = new Font("Serif", Font.ITALIC | Font.BOLD, 20)
		}
	}

	val button = new Button(Action("Ok") { this.close })

	contents = new BorderPanel {
		add(editScroll, BorderPanel.Position.Center)
		add(button, BorderPanel.Position.South)
	}

	visible = true
}
