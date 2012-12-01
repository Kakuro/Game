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
import java.awt.Font

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

	// add ComboBox for look and feel 
	val list = new ListBuffer[String]()
	for (info <- javax.swing.UIManager.getInstalledLookAndFeels)
		list += info.getClassName()

	val comboBox = new ComboBox(list) {
		listenTo(selection)
		reactions += {
			case e: SelectionChanged => {
				val selected = selection.item
				try { javax.swing.UIManager.setLookAndFeel(selected) }
				catch { case _ => javax.swing.UIManager.setLookAndFeel(javax.swing.UIManager.getSystemLookAndFeelClassName()); }
				drawNew
			}
		}

	}

	var cells = Array.ofDim[TextField](controller.model.row__, controller.model.column__)

	// creating the grid panel for the play field
	def gridPanel = new GridPanel(controller.model.row__, controller.model.column__) {
		contents.clear
		border = LineBorder(java.awt.Color.BLACK, 30)
		background = java.awt.Color.BLACK

		for (row <- 0 until controller.model.row__; column <- 0 until controller.model.column__) {

			cells(row)(column) = new TextField
			cells(row)(column).horizontalAlignment = Alignment.Center
			cells(row)(column).border = LineBorder(java.awt.Color.GRAY, 5)
			cells(row)(column).foreground = java.awt.Color.GRAY
			cells(row)(column).font = new Font("Serif", Font.ITALIC | Font.BOLD, 30)

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
					cells(row)(column).visible = false
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

	reactions += {
		case e: ChangeCell => drawNew
		case e: NewPlayField => drawNew
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
				case cellFalseColumn(column, rowFrom, rowTo) => //@todo
				case cellTrueRow(row, columnFrom, columnTo) => //@todo
				case cellTrueColumn(column, rowFrom, rowTo) => //@todo
				case cellMultipleRow(row, columnFrom, columnTo) => //@todo
				case cellMultipleColumn(column, rowFrom, rowTo) => //@todo
				case _ =>
			})
		}
	}

	contents = new BorderPanel {
		add(comboBox, BorderPanel.Position.South)
		add(gridPanel, BorderPanel.Position.Center)
	}

	visible = true

	def drawNew {
		contents = new BorderPanel {
			add(comboBox, BorderPanel.Position.South)
			add(gridPanel, BorderPanel.Position.Center)
		}
	}
}