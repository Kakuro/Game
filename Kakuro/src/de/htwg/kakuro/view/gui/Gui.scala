package de.htwg.kakuro.view.gui

import scala.swing._
import scala.swing.event._
import scala.io.Source
import de.htwg.kakuro.controller.KakuroController
import de.htwg.kakuro.controller.ChangeCell
import de.htwg.kakuro.controller.NewPlayField
import de.htwg.kakuro.controller.CheckCell
import java.io.FilenameFilter
import javax.swing.filechooser.FileNameExtensionFilter
import java.io.File
import javax.sound.sampled.Line
import scala.swing.Swing.LineBorder
import _root_.de.htwg.kakuro.model.Cell
import _root_.de.htwg.kakuro.model.SumCell
import de.htwg.kakuro.controller.ChangeCell
import scala.collection.mutable.ListBuffer
import scala.io.Position

class Gui(controller: KakuroController) extends Frame {
	listenTo(controller)

	// size and position
	val framewidth = 640
	val frameheight = 480
	val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
	location = new java.awt.Point((screenSize.width - framewidth) / 2, (screenSize.height - frameheight) / 2)
	minimumSize = new java.awt.Dimension(framewidth, frameheight)

	// titel und icon
	title = "Kakuro"
	iconImage = toolkit.getImage("data/icon.png")

	// add ListView
	val list = new ListBuffer[String]()
	for (info <- javax.swing.UIManager.getInstalledLookAndFeels)
		list += info.getClassName()
	val listView = new ListView(list) {
		listenTo(mouse.clicks)
		reactions += {
			case e: MouseClicked => {
				println(e)
			}
		}
	}
	val scrollPane = new ScrollPane()
	scrollPane.contents = listView

	// Look and Feel
	try {
		javax.swing.UIManager.setLookAndFeel(new javax.swing.plaf.metal.MetalLookAndFeel)
	} catch {
		case _ => javax.swing.UIManager.setLookAndFeel(javax.swing.UIManager.getSystemLookAndFeelClassName());
	}

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
				case c: Cell => {
					cells(row)(column).text = c.toString
					cells(row)(column).name = "Cell " + row + "," + column
				}
				case c: SumCell => {
					cells(row)(column).text = c.toString
					cells(row)(column).background = java.awt.Color.YELLOW
					cells(row)(column).editable = false
					cells(row)(column).name = "SumCell " + row + "," + column
				}
				case _ => {
					cells(row)(column).background = java.awt.Color.BLACK
					cells(row)(column).editable = false
					cells(row)(column).name = "-- " + row + "," + column
				}
			}
			contents += cells(row)(column)
			listenTo(cells(row)(column))

		}

		reactions += {
			//			case KeyPressed(name, Key.Enter, mod, value) => println("Ok, searching DB for input " + name.asInstanceOf[TextField].text)
			case EditDone(textField) =>
				val cell = "Cell ([1-9]*[0-9]*[0-9]),([1-9]*[0-9]*[0-9])".r
				val cellValue = "([1-9]*[0-9]*[0-9])".r

				textField.name.split("	").toList.filter(c => c != ' ').map(c => c match {
					case cell(row, column) => {
						textField.text.split("	").toList.filter(c => c != ' ').map(c => c match {
							case cellValue(value) => controller.setValue(row.toInt, column.toInt, value.toInt)
							case _ =>
						})
					}
					case _ =>
				})
			case _ =>
		}
	}

	contents = new BorderPanel {
		add(scrollPane, BorderPanel.Position.South)
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
					} catch { case e: Exception => Dialog.showMessage(this, "Exception: \n" + e.getMessage) }
					case FileChooser.Result.Cancel =>
				}
			}) { mnemonic = Key.L }
			contents += new MenuItem(Action("Quit") { System.exit(0) }) { mnemonic = Key.Q }
		}
	}

	visible = true

	reactions += {
		case e: ChangeCell => repaint
		case e: NewPlayField => repaint
		case e: CheckCell => {

			val cellFalseRow = "False row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellFalseColumn = "False column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			val cellTrueRow = "True row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellTrueColumn = "True column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			val cellMultipleRow = "Multiple number: row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellMultipleColumn = "Multiple number:  column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			e.result.split("	").toList.filter(c => c != ' ').map(c => c match {
				case cellFalseRow(row, columnFrom, columnTo) =>

					for (column <- columnFrom.toInt until columnTo.toInt + 1) {

						cells(row.toInt)(column).background = java.awt.Color.RED
						cells(row.toInt)(column).repaint
					}
				case cellFalseColumn(column, rowFrom, rowTo) =>
				case cellTrueRow(row, columnFrom, columnTo) =>
				case cellTrueColumn(column, rowFrom, rowTo) =>
				case cellMultipleRow(row, columnFrom, columnTo) =>
				case cellMultipleColumn(column, rowFrom, rowTo) =>
				case _ =>
			})
		}
	}

	override def repaint {
		contents = new BorderPanel {
			add(gridPanel, BorderPanel.Position.Center)
		}
	}
}