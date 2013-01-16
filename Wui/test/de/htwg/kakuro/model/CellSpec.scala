package de.htwg.kakuro.model

import org.specs2.mutable._

class CellSpec extends SpecificationWithJUnit {

	"A new Cell on place (0,0) with value 0" should {
		val cell = new Cell(0, 0)
		
		"have value 0" in {
			cell.value must be_==(0)
		}

		"have status 0" in {
			cell.status must be_==(0)
		}

		"equal 0" in {
			(cell == 0) must beTrue
		}

		"generate a String of the form ''" in {
			cell.toString must be_==("")
		}
	}

	"A new Cell on place (0,0) with value 1" should {
		val cell = new Cell(0, 0)
		cell <== 1

		"have value 0 " in {
			cell.value must be_==(1)
		}

		"have status 0 " in {
			cell.status must be_==(0)
		}

		"equal 0 " in {
			(cell == 1) must beTrue
		}

		"generate a String of the form ''" in {
			cell.toString must be_==("1")
		}
	}
}