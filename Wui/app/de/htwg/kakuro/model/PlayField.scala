package de.htwg.kakuro.model

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Random
/**
 * Class PlayField: Describes the playfield
 *
 * @param size: size of the playfield
 */
class PlayField() {

	var cells = Array.ofDim[AbstractCell](0, 0)
	var scoreList: List[String] = List()
	var row__ = 0
	var column__ = 0
	
	// load random default level
	val srandom = new Random(System.currentTimeMillis())
	val range = 1 to 3
	var fileName = "app/data/default"+ range(srandom.nextInt(range length)) +".ini"
	load(fileName)

	// resetting the complete field
	def reset = {
		load(fileName)
	}

	// checking a array cell of numbers
	private def multipleNumber(array: Array[Int]): Boolean = {
		var multipleNum = false
		var tempArraySum = array.sortWith(_ < _)
		for (i <- 0 to array.length - 1)
			for (j <- i + 1 to array.length - 1)
				if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
					multipleNum = true
		multipleNum
	}

	private def stringResult_Row(arraySum: Array[Int], row: Int, tempSum: Int, tempColumn: Int, column: Int, sum: Int): String = {
		var result: String = ""

		if (multipleNumber(arraySum)) {
			result = "Multiple number: row " + row + ", column from " + tempColumn + " to " + (column - 1)
		} else {
			if ((sum == tempSum) && (sum != 0))
				result = "True row " + row + ", column from " + tempColumn + " to " + (column - 1)
			if ((sum != tempSum) && (tempSum != 0))
				result = "False row " + row + ", column from " + tempColumn + " to " + (column - 1)
		}
		result
	}

	private def stringResult_Column(arraySum: Array[Int], row: Int, tempSum: Int, tempRow: Int, column: Int, sum: Int): String = {
		var result: String = ""

		if (multipleNumber(arraySum)) {
			result = "Multiple number: column " + column + ", row from " + tempRow + " to " + (row - 1)
		} else {
			if ((sum == tempSum) && (sum != 0))
				result = "True column " + column + ", row from " + tempRow + " to " + (row - 1)
			if ((sum != tempSum) && (tempSum != 0))
				result = "False column " + column + ", row from " + tempRow + " to " + (row - 1)
		}
		result
	}

	// checking the complete field with corresponding sums
	def check: (ListBuffer[String], Boolean) = {
		var sum = 0
		var tempSum = 0
		var arraySum = new Array[Int](column__)
		var result = new ListBuffer[String]
		var tempColumn = 0
		var tempRow = 0
		var isCellValid = true
		var cellsCount = 0

		// check all rows
		for (row <- 0 until row__) {
			for (column <- 0 until column__) {
				cells(row)(column) match {
					case c: Cell =>
						tempSum += c.value
						arraySum(column) = c.value
						if (c.value == 0) isCellValid = false

					case c: SumCell =>
						if (tempSum != 0 && isCellValid)
							result += stringResult_Row(arraySum, row, tempSum, tempColumn, column, sum)
						tempSum = 0
						arraySum = new Array[Int](column__)
						isCellValid = true
						tempColumn = column + 1
						sum = c.columnSum

						if (c.rowSum != 0)
							cellsCount += 1
						if (c.columnSum != 0)
							cellsCount += 1

					case _ =>
						if (tempSum != 0 && isCellValid)
							result += stringResult_Row(arraySum, row, tempSum, tempColumn, column, sum)
						tempSum = 0
						arraySum = new Array[Int](column__)
						isCellValid = true
				}
			}

			if (tempSum != 0 && isCellValid)
				result += stringResult_Row(arraySum, row, tempSum, tempColumn, column__, sum)
			isCellValid = true
			tempSum = 0
			arraySum = new Array[Int](column__)
		}

		// check all column
		for (column <- 0 until column__) {
			for (row <- 0 until row__) {
				cells(row)(column) match {
					case c: Cell =>
						tempSum += c.value
						arraySum(row) = c.value
						if (c.value == 0) isCellValid = false

					case c: SumCell =>
						if (tempSum != 0 && isCellValid)
							result += stringResult_Column(arraySum, row, tempSum, tempRow, column, sum)
						isCellValid = true
						tempSum = 0
						arraySum = new Array[Int](row__)
						tempRow = row + 1
						sum = c.rowSum

					case _ =>
						if (tempSum != 0 && isCellValid)
							result += stringResult_Column(arraySum, row, tempSum, tempRow, column, sum)
						tempSum = 0
						arraySum = new Array[Int](row__)
						isCellValid = true
				}
			}

			if (tempSum != 0 && isCellValid)
				result += stringResult_Column(arraySum, row__, tempSum, tempRow, column, sum)
			isCellValid = true
			tempSum = 0
			arraySum = new Array[Int](row__)
		}

		var allCellsOk = false
		val allCells = result.sortWith(_ < _)
		var cellsCountHelp = 0
		val cellTrueRow = "True row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
		val cellTrueColumn = "True column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

		allCells.foreach(f => f.split("	").toList.filter(c => c != ' ').map(c => c match {
			case cellTrueRow(row, columnFrom, columnTo) => cellsCountHelp += 1
			case cellTrueColumn(column, rowFrom, rowTo) => cellsCountHelp += 1
			case _ =>
		}))

		if (cellsCount == cellsCountHelp)
			allCellsOk = true

		val cellFalseRow = "False row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
		val cellFalseColumn = "False column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

		val cellMultipleRow = "Multiple number: row ([1-9]*[0-9]*[0-9]), column from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r
		val cellMultipleColumn = "Multiple number: column ([1-9]*[0-9]*[0-9]), row from ([1-9]*[0-9]*[0-9]) to ([1-9]*[0-9]*[0-9])".r

		cells.foreach { row =>
			row.foreach { column =>
				column match {
					case c: Cell => c.status = 0
					case _ =>
				}
			}
		}
		
		result.sortWith(_ < _).foreach(f => f.split("	").toList.filter(c => c != ' ').map(c => c match {
			case cellFalseRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, 3)
			case cellFalseColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, 3)
			case cellTrueRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, 1)
			case cellTrueColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, 1)
			case cellMultipleRow(row, columnFrom, columnTo) => helpCheck(true, row.toInt, columnFrom.toInt, columnTo.toInt, 2)
			case cellMultipleColumn(column, rowFrom, rowTo) => helpCheck(false, column.toInt, rowFrom.toInt, rowTo.toInt, 2)
			case _ =>
		}))

		(result.sortWith(_ < _), allCellsOk)
	}

	private def helpCheck(rowOr: Boolean, rowOrColumn: Int, from: Int, to: Int, _status: Int) {

		if (rowOr) {
			for (column <- from until to + 1) {
				cells(rowOrColumn)(column) match {
					case c: Cell =>
						c.status = _status
				}
			}
		} else {
			for (row <- from until to + 1) {
				cells(row)(rowOrColumn) match {
					case c: Cell =>
						c.status = _status
				}
			}
		}
	}

	def load(name: String) = {
		fileName = name
		val rowCount = "#/(.*)".r
		val columnCount = "(.*)/#".r
		val column_row_Count = "(.*)/(.*)".r
		val size_ = "size:(.*)x(.*)".r
		val cellVal = "([1-9]*[0-9]*[0-9])".r
		var row = -1;
		var column = 0;
		var setSize = false;

		try
			for (line <- Source.fromFile(fileName).getLines()) {
				line.split("	").toList.filter(c => c != ' ').map(c => c match {
					case "#/#" =>
						column += 1
					case "##" =>
						cells(row)(column) = new Cell(row, column)
						column += 1
					case cellVal(value) =>
						cells(row)(column) = new Cell(row, column)
						cells(row)(column) match { case c: Cell => c <== value.toInt }
						column += 1
					case size_(row_, column_) =>
						cells = Array.ofDim[AbstractCell](row_.toInt, column_.toInt)
						row__ = row_.toInt
						column__ = column_.toInt
						setSize = true
					case rowCount(value) =>
						cells(row)(column) = new SumCell(row, column, 0, value.toInt)
						column += 1
					case columnCount(value) =>
						cells(row)(column) = new SumCell(row, column, value.toInt, 0)
						column += 1
					case column_row_Count(yVal, xVal) =>
						cells(row)(column) = new SumCell(row, column, yVal.toInt, xVal.toInt)
						column += 1
					case _ =>
				})
				if (setSize == true) row += 1
				column = 0
			}
		catch {
			case e: java.io.FileNotFoundException => println("File not found or pathname is false.")
		}
	}

	override def toString = {
		var result = ""
		cells.foreach { row =>
			row.foreach { column =>
				column match {
					case c: Cell => result += c.toString + "\t"
					case c: SumCell => result += c.toString + "\t"
					case _ => result += " --\t"
				}
				result += "|"
			}
			result += "\n"
		}
		result
	}
}