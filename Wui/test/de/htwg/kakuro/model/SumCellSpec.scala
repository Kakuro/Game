package de.htwg.kakuro.model

import org.specs2.mutable._

class SumCellSpec extends SpecificationWithJUnit {

	"A new Cell on place (0,0) with value (0/0)" should {
		val cellSum = new SumCell(0, 0, 0, 0)

		"have row sum 0 " in {
			cellSum.rowSum must be_==(0)
		}

		"have column sum 0 " in {
			cellSum.columnSum must be_==(0)
		}

		"equal row with 0 " in {
			(cellSum =- 0) must beTrue
		}

		"equal column with 0 " in {
			(cellSum =| 0) must beTrue
		}

		"generate a String of the form ''" in {
			cellSum.toString must be_==("\\")
		}
	}

	"A new Cell on place (0,0) with value (2/2)" should {
		val cellSum = new SumCell(0, 0, 2, 2)

		"have row sum 2" in {
			cellSum.rowSum must be_==(2)
		}

		"have column sum 2" in {
			cellSum.columnSum must be_==(2)
		}

		"equal row with 2" in {
			(cellSum =- 2) must beTrue
		}

		"equal column with 2 " in {
			(cellSum =| 2) must beTrue
		}

		"generate a String of the form ''" in {
			cellSum.toString must be_==("2\\2")
		}
	}
}