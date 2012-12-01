package de.htwg.kakuro.view.gui

import scala.swing._
import scala.swing.event._
import scala.io.Source
import de.htwg.kakuro.controller.KakuroController
import de.htwg.kakuro.controller.ChangeCell
import de.htwg.kakuro.controller.NewPlayField
import de.htwg.kakuro.controller.FalseCell
import java.io.FilenameFilter
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.File
import javax.sound.sampled.Line
import scala.swing.Swing.LineBorder
import _root_.de.htwg.kakuro.model.Cell
import _root_.de.htwg.kakuro.model.SumCell
import de.htwg.kakuro.controller.ChangeCell

class Gui(controller : KakuroController) extends Frame {
	listenTo(controller)
	title = "Kakuro"

	var cells = Array.ofDim[TextField](controller.model.row__, controller.model.column__)

	// creating the grid panel for the play field
	def gridPanel = new GridPanel(controller.model.row__, controller.model.column__) {
		border = LineBorder(java.awt.Color.CYAN, 4)
		//		background = java.awt.Color.RED
		contents.clear
		for (row <- 0 until controller.model.row__; column <- 0 until controller.model.column__) {

			cells(row)(column) = new TextField
			cells(row)(column).horizontalAlignment = Alignment.Left
			controller.model.cells(row)(column) match {
				case c : Cell => {
					cells(row)(column).text = c.toString
					cells(row)(column).name = "Cell " + row + " " + column
				}
				case c : SumCell => {
					cells(row)(column).text = c.toString
					cells(row)(column).background = java.awt.Color.YELLOW
					cells(row)(column).editable = false
					cells(row)(column).name = "SumCell " + row + " " + column
				}
				case _ => {
					cells(row)(column).text = "--"
					cells(row)(column).background = java.awt.Color.BLACK
					cells(row)(column).editable = false
					cells(row)(column).name = "-- " + row + " " + column
				}
			}
			contents += cells(row)(column)
			listenTo(cells(row)(column))
			val cellVal = "([1-9]*[0-9]*[0-9])".r

		}

		reactions += {
			case KeyPressed(name, Key.Enter, mod, value) => println("Ok, searching DB for input "+ name.asInstanceOf[TextField].text)
			case KeyReleased(name, Key.Space, _, _) => println("Ok, searching DB for input "+ name.asInstanceOf[TextField].text)
			case EditDone(textField) => println("Ok, searching DB for input "+ textField.text)
			case e: ActionEvent => println("e.toString")
			case _ =>
			//				{

			//				println(name + "\n" + mod + "\n" + value)

			//				cells(row)(column).text.split(" ").toList.filter(c => c != ' ').map(c => c match {
			//					case cellVal(value) =>
			//						println(value)
			//						controller.setValue(row, column, value.toInt)
			//					case _ =>
			//
			//				})
			//			}
		}

	}

	contents = new BorderPanel {
		add(gridPanel, BorderPanel.Position.Center)
	}

	// creating a menu Bar
	menuBar = new MenuBar {
		contents += new Menu("File") {
			mnemonic = Key.F
			contents += new MenuItem(Action("New") { controller.load("data/easy.ini") }) { mnemonic = Key.N }
			contents += new MenuItem(Action("Check") { controller.check }) { mnemonic = Key.C }
			contents += new MenuItem(Action("Reset") { controller.reset }) { mnemonic = Key.R }
			contents += new MenuItem(Action("Load") {
				val fileDialog = new FileChooser() { fileFilter = new FileNameExtensionFilter("Kakuro", "ini") }
				fileDialog.title = "Load new Game"

				fileDialog.showOpenDialog(this) match {
					case FileChooser.Result.Approve => try {
						if (fileDialog.selectedFile.exists()) {
							controller.load(fileDialog.selectedFile.getAbsoluteFile().toString())
						}
					} catch { case e : Exception => Dialog.showMessage(this, "Exception: \n" + e.getMessage) }
					case FileChooser.Result.Cancel =>
				}
			}) { mnemonic = Key.L }
			contents += new MenuItem(Action("Quit") { System.exit(0) }) { mnemonic = Key.Q }
		}
	}

	visible = true

	reactions += {
		case e : ChangeCell => repaint
		case e : NewPlayField => repaint
		case e : FalseCell => println(e.result)
	}

	override def repaint {
		contents = new BorderPanel {
			add(gridPanel, BorderPanel.Position.Center)
		}
	}
}