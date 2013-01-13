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
import javax.swing.Timer
import java.util.Timer
import Swing._
import de.htwg.kakuro.controller.CheckCellsResult

class Gui(controller: KakuroController) extends Frame {
	listenTo(controller)

	class Style {
		var background: java.awt.Color = java.awt.Color.BLACK
		var editCell: java.awt.Color = java.awt.Color.WHITE
		var editCellForeground: java.awt.Color = java.awt.Color.BLACK
		var nonCell: java.awt.Color = java.awt.Color.YELLOW
		var noneCellForeground: java.awt.Color = java.awt.Color.GRAY
	}
	private var style = new Style

	// size and position
	val framewidth = 640
	val frameheight = 480
	val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
	location = new java.awt.Point((screenSize.width - framewidth) / 2, (screenSize.height - frameheight) / 2)
	minimumSize = new java.awt.Dimension(framewidth, frameheight)

	// titel und icon
	title = "Kakuro"
	iconImage = toolkit.getImage("app/data/icon.png")

	var cells = Array.ofDim[TextField](controller.model.row__, controller.model.column__)

	// creating the grid panel for the play field
	def gridPanel = new GridPanel(controller.model.row__, controller.model.column__) {
		contents.clear
		border = LineBorder(style.background, 30)
		background = style.background

		for (row <- 0 until controller.model.row__; column <- 0 until controller.model.column__) {

			cells(row)(column) = new TextField
			cells(row)(column).horizontalAlignment = Alignment.Center
			cells(row)(column).border = LineBorder(java.awt.Color.GRAY, 5)
			cells(row)(column).foreground = style.noneCellForeground
			cells(row)(column).font = new Font("Serif", Font.ITALIC | Font.BOLD, 30)

			controller.model.cells(row)(column) match {
				case c: Cell => {
					cells(row)(column).foreground = style.editCellForeground
					cells(row)(column).background = style.editCell
					cells(row)(column).font = new Font("Serif", Font.BOLD, 30)
					cells(row)(column).text = c.toString
					cells(row)(column).name = "Cell " + row + "," + column
				}
				case c: SumCell => {
					cells(row)(column).text = c.toString
					cells(row)(column).background = style.nonCell
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
				val cellValue = "([1-9])".r

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
			contents += new MenuItem(Action("New") { controller.load(controller.model.fileName) }) { mnemonic = Key.N }
			contents += new MenuItem(Action("Load") {
				val fileDialog = new FileChooser(new File(System.getProperty("user.dir"))) { fileFilter = new FileNameExtensionFilter("Kakuro", "ini") }
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
		contents += new Menu("Style") {
			mnemonic = Key.S
			contents += new MenuItem(Action("Style 1") {
				style.background = java.awt.Color.BLACK
				style.editCell = java.awt.Color.WHITE
				style.editCellForeground = java.awt.Color.BLACK
				style.nonCell = java.awt.Color.YELLOW
				style.noneCellForeground = java.awt.Color.GRAY

				drawNew
			}) {}

			contents += new MenuItem(Action("Style 2") {
				style.background = java.awt.Color.YELLOW
				style.editCell = java.awt.Color.CYAN
				style.editCellForeground = java.awt.Color.BLACK
				style.nonCell = java.awt.Color.BLUE
				style.noneCellForeground = java.awt.Color.GRAY

				drawNew
			}) {}

		}
		contents += new Menu("Help") {
			mnemonic = Key.H
			contents += new MenuItem(Action("About Kakuro") {
				Dialog.showMessage(this,
					"- The structure is similar to a crossword puzzle.\n" +
					"- Instead of letters numerals.\n" +
					"- Instead of word definitions are defined sum.\n\n" +
					"- Each total must consist only of the digits 1 to 9.\n" +
					"- In each sum (cell) can occur each digit only once.\n" +
					"- At each place, only one digit can be entered.\n",
					"Rules")
			}) { mnemonic = Key.A }
		}
	}

	def helpCheck(rowOr: Boolean, rowOrColumn: Int, from: Int, to: Int, color: java.awt.Color) {

		if (rowOr) {
			for (column <- from until to + 1) {
				cells(rowOrColumn)(column).background = color
				cells(rowOrColumn)(column).repaint
			}
		} else {
			for (row <- from until to + 1) {
				cells(row)(rowOrColumn).background = color
				cells(row)(rowOrColumn).repaint
			}
		}
	}

	reactions += {
		case e: ChangeCell => drawNew
		case e: NewPlayField => drawNew
		case e: CheckCellsResult => {
			timer.stop
			if (e.result == true) {
				val r = Dialog.showInput(contents.head, "Check Ok", initial = "Enter your name")
				r match {
					case Some(s) =>
						controller.model.scoreList ::= "Name: " + s + ", with Time : " + timeStamp.toString + " sec."
						timeStamp = 0
					case None =>
				}
			} else
//				Dialog.showMessage(contents.head, "Check Not Ok", title = "Check")
			timer.start
		}
		case e: CheckCell => {

			val cellFalseRow = "False row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellFalseColumn = "False column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			val cellTrueRow = "True row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellTrueColumn = "True column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			val cellMultipleRow = "Multiple number: row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
			val cellMultipleColumn = "Multiple number: column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

			e.result.split("	").toList.filter(c => c != ' ').map(c => c match {
				case cellFalseRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, java.awt.Color.RED)
				case cellFalseColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, java.awt.Color.RED)
				case cellTrueRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, java.awt.Color.GREEN)
				case cellTrueColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, java.awt.Color.GREEN)
				case cellMultipleRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, java.awt.Color.ORANGE)
				case cellMultipleColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, java.awt.Color.ORANGE)
				case _ =>
			})
		}
	}

	// BoxPanel on the top with two buttons and a label
	var timeStamp = 0;
	var timer: javax.swing.Timer = null
	def boxPanel = new BoxPanel(Orientation.Horizontal) {

		background = style.background

		// timer
		val label = new Label {
			font = new Font("Serif", Font.BOLD, 20)
			Alignment.Right
			foreground = java.awt.Color.WHITE
		}

		val timerlistener = new java.awt.event.ActionListener() {
			def actionPerformed(evt: java.awt.event.ActionEvent) {
				timeStamp += 1
				label.text = "Time : " + timeStamp.toString + " sec."
			}
		}

		if (timer != null) timer.stop()

		timer = new javax.swing.Timer(1000, timerlistener)
		timer.start()

		contents += new Button(Action("Check") { controller.check })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Reset") { controller.reset; timeStamp = 0 })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Score") { new Score(controller.model.scoreList) })
		contents += Swing.Glue
		contents += label
		border = Swing.EmptyBorder(10, 10, 10, 10)
	}

	def flowPanel = new FlowPanel {
		background = style.background
		border = Swing.EmptyBorder(10, 10, 10, 10)
		contents += new Button(Action("Level 1") { controller.load("app/data/level1.ini"); timeStamp = 0 })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Level 2") { controller.load("app/data/level2.ini"); timeStamp = 0 })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Level 3") { controller.load("app/data/level3.ini"); timeStamp = 0 })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Level 4") { controller.load("app/data/level4.ini"); timeStamp = 0 })
		contents += Swing.HStrut(20)
		contents += new Button(Action("Level solved") { controller.load("app/data/solved.ini"); timeStamp = 0 })
	}

	contents = new BorderPanel {
		add(boxPanel, BorderPanel.Position.North)
		add(gridPanel, BorderPanel.Position.Center)
		add(flowPanel, BorderPanel.Position.South)
	}

	visible = true

	def drawNew {
		cells = Array.ofDim[TextField](controller.model.row__, controller.model.column__)
		contents = new BorderPanel {
			add(boxPanel, BorderPanel.Position.North)
			add(gridPanel, BorderPanel.Position.Center)
			add(flowPanel, BorderPanel.Position.South)
		}
	}
}