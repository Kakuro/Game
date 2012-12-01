package de.htwg.kakuro.model

/**
 * Class SumCell: Describes the sum of all cells in horizontal or vertical direction.
 * 
 * @param row: 		row in the field
 * @param column:	column in the field
 * @row_Sum:		sum in horizontal direction
 * @column_Sum:		sum in vertical direction
 */
class SumCell(row: Int, column: Int, row_Sum: Int, column_Sum: Int) extends AbstractCell{

	var rowSum = row_Sum
	var columnSum = column_Sum

	//compare the row and column value with set value
	def =|(c: Int) = if (column_Sum == c) true else false
	def =-(r: Int) = if (row_Sum == r) true else false
	
	def celltoString = "Cell (" + row + ", " + column + ") with row sum = " + row_Sum + " and column sum = " + column_Sum

	private def toString_Row = row_Sum.toString.replaceAll("^0", " ")
	private def toString_Column = column_Sum.toString.replaceAll("^0", " ")
	override def toString = toString_Row + "\\" + toString_Column
}