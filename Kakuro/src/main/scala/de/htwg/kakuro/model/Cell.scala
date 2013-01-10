package de.htwg.kakuro.model
/**
 * Class Cell: Describes visible or invisible cell in the field
 *
 * @param row: 		row in the field
 * @param column:	column in the field
 * @param cellType:	type of the cell (Visible, Non)
 */
class Cell(row: Int, column: Int) extends AbstractCell {

	var value = 0

	//set value
	def <==(v: Int) = value = v

	//compare the value with the set value
	def ==(v: Int) = if (value == v) true else false

	def celltoString = "Visible cell (" + row + ", " + column + ") with Value = " + value
	override def toString = value.toString.replaceAll("^0", "")
}