package de.htwg.kakuro.model

import scala.io.Source
import scala.collection.mutable.ListBuffer
/**
 * Class PlayField: Describes the playfield
 *
 * @param size: size of the playfield
 */
class PlayField() {

	var cells = Array.ofDim[AbstractCell](0, 0)
	var row__ = 0
	var column__ = 0

	load("data/default.ini")

	// resetting the complete field
	def reset = {
		for (row <- 0 until row__; column <- 0 until column__) {
			cells(row)(column) match {
				case c: Cell => c <== 0
				case c: SumCell => // do nothing
				case _ => // do nothing
			}
		}
	}

	// checking the complete field with corresponding sums
	def check: ListBuffer[String] = {
		var sum = 0
		var tempSum = 0
		var arraySum = new Array[Int](column__)
		var resultTrue = new ListBuffer[String]
		var resultFalse = new ListBuffer[String]
		var resultMultple = new ListBuffer[String]
		var result = new ListBuffer[String]
		var tempColumn = 0
		var tempRow = 0
		var isCellValid = true

		// check all rows
		for (row <- 0 until row__) {
			for (column <- 0 until column__) {
				cells(row)(column) match {
					case c: Cell =>
						tempSum += c.value
						arraySum(column) = c.value

						if (c.value == 0) isCellValid = false

					case c: SumCell =>

						if (tempSum != 0) {

							println("Cell valid ==> " + isCellValid + " : row " + row + ", column from " + tempColumn + " to " + (column - 1))

							var multipleNum = false
							var tempArraySum = arraySum.sortWith(_ < _)
							for (i <- 0 to arraySum.length - 1)
								for (j <- i + 1 to arraySum.length - 1)
									if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
										multipleNum = true

							if (multipleNum)
								resultMultple += "Multiple number: row " + row + ", column from " + tempColumn + " to " + (column - 1)
							else {
								if ((sum == tempSum) && (sum != 0))
									resultTrue += "True row " + row + ", column from " + tempColumn + " to " + (column - 1)
								if ((sum != tempSum) && (tempSum != 0))
									resultFalse += "False row " + row + ", column from " + tempColumn + " to " + (column - 1)
							}
							tempSum = 0
							arraySum = new Array[Int](column__)
						}
						isCellValid = true
						tempColumn = column + 1
						sum = c.columnSum

					case _ =>
						if (tempSum != 0) {

							println("Cell valid ==> " + isCellValid + " : row " + row + ", column from " + tempColumn + " to " + (column - 1))

							var multipleNum = false
							var tempArraySum = arraySum.sortWith(_ < _)
							for (i <- 0 to arraySum.length - 1)
								for (j <- i + 1 to arraySum.length - 1)
									if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
										multipleNum = true

							if (multipleNum)
								resultMultple += "Multiple number: row " + row + ", column from " + tempColumn + " to " + (column - 1)
							else {
								if ((sum == tempSum) && (sum != 0))
									resultTrue += "True row " + row + ", column from " + tempColumn + " to " + (column - 1)
								if ((sum != tempSum) && (tempSum != 0))
									resultFalse += "False row " + row + ", column from " + tempColumn + " to " + (column - 1)
							}
							tempSum = 0
							arraySum = new Array[Int](column__)
						}
						isCellValid = true
				}
			}

			if (tempSum != 0) {

				println("Cell valid ==> " + isCellValid + " : row " + row + ", column from " + tempColumn + " to " + (column__ - 1))

				var multipleNum = false
				var tempArraySum = arraySum.sortWith(_ < _)
				for (i <- 0 to arraySum.length - 1)
					for (j <- i + 1 to arraySum.length - 1)
						if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
							multipleNum = true

				if (multipleNum)
					resultMultple += "Multiple number: row " + row + ", column from " + tempColumn + " to " + (column__ - 1)
				else {
					if ((sum == tempSum) && (sum != 0))
						resultTrue += "True row " + row + ", column from " + tempColumn + " to " + (column__ - 1)
					if ((sum != tempSum) && (tempSum != 0))
						resultFalse += "False row " + row + ", column from " + tempColumn + " to " + (column__ - 1)
				}
			}
			isCellValid = true
			tempSum = 0
			arraySum = new Array[Int](column__)
		}

		tempSum = 0
		arraySum = new Array[Int](row__)

		isCellValid = true

		// check all column
		for (column <- 0 until column__) {
			for (row <- 0 until row__) {
				cells(row)(column) match {
					case c: Cell =>
						tempSum += c.value
						arraySum(row) = c.value

						if (c.value == 0) isCellValid = false

					case c: SumCell =>

						if (tempSum != 0) {

							println("Cell valid ==> " + isCellValid + " : column " + column + ", row from " + tempRow + " to " + (row - 1))

							var multipleNum = false
							var tempArraySum = arraySum.sortWith(_ < _)
							for (i <- 0 to arraySum.length - 1)
								for (j <- i + 1 to arraySum.length - 1)
									if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
										multipleNum = true

							if (multipleNum)
								resultMultple += "Multiple number: column " + column + ", row from " + tempRow + " to " + (row - 1)
							else {
								if ((sum == tempSum) && (sum != 0))
									resultTrue += "True column " + column + ", row from " + tempRow + " to " + (row - 1)
								if ((sum != tempSum) && (tempSum != 0))
									resultFalse += "False column " + column + ", row from " + tempRow + " to " + (row - 1)
							}
							isCellValid = true
							tempSum = 0
							arraySum = new Array[Int](row__)
						}
						tempRow = row + 1
						sum = c.rowSum

					case _ =>
						if (tempSum != 0) {

							println("Cell valid ==> " + isCellValid + " : column " + column + ", row from " + tempRow + " to " + (row - 1))

							var multipleNum = false
							var tempArraySum = arraySum.sortWith(_ < _)
							for (i <- 0 to arraySum.length - 1)
								for (j <- i + 1 to arraySum.length - 1)
									if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
										multipleNum = true

							if (multipleNum)
								resultMultple += "Multiple number: column " + column + ", row from " + tempRow + " to " + (row - 1)
							else {
								if ((sum == tempSum) && (sum != 0))
									resultTrue += "True column " + column + ", row from " + tempRow + " to " + (row - 1)
								if ((sum != tempSum) && (tempSum != 0))
									resultFalse += "False column " + column + ", row from " + tempRow + " to " + (row - 1)
							}
							tempSum = 0
							arraySum = new Array[Int](row__)
						}
						isCellValid = true
				}
			}

			if (tempSum != 0) {

				println("Cell valid ==> " + isCellValid + " : column " + column + ", row from " + tempRow + " to " + (row__ - 1))

				var multipleNum = false
				var tempArraySum = arraySum.sortWith(_ < _)
				for (i <- 0 to arraySum.length - 1)
					for (j <- i + 1 to arraySum.length - 1)
						if ((tempArraySum(i) == tempArraySum(j)) && (tempArraySum(i) != 0))
							multipleNum = true

				if (multipleNum)
					resultMultple += "Multiple number: column " + column + ", row from " + tempRow + " to " + (row__ - 1)
				else {
					if ((sum == tempSum) && (sum != 0))
						resultTrue += "True column " + column + ", row from " + tempRow + " to " + (row__ - 1)
					if ((sum != tempSum) && (tempSum != 0))
						resultFalse += "False column " + column + ", row from " + tempRow + " to " + (row__ - 1)
				}
			}
			isCellValid = true
			tempSum = 0
			arraySum = new Array[Int](row__)
		}

		resultFalse.foreach(c => result += c)
		resultTrue.foreach(c => result += c)
		resultMultple.foreach(c => result += c)
		result
	}

	def load(name: String) = {
		val rowCount = "#/(.*)".r
		val columnCount = "(.*)/#".r
		val column_row_Count = "(.*)/(.*)".r
		val size_ = "size:(.*)x(.*)".r
		val cellVal = "([1-9]*[0-9]*[0-9])".r
		var row = -1;
		var column = 0;
		var setSize = false;

		try
			for (line <- Source.fromFile(name).getLines()) {
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