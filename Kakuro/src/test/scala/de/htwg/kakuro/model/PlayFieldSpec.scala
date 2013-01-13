package de.htwg.kakuro.model

import org.specs2.mutable._
import scala.collection.mutable.ListBuffer

class PlayFieldSpec extends SpecificationWithJUnit {

	"A new PlayField with defalut field" should {
		val field = new PlayField

		"have row and coumln != 0" in {
			(field.row__ != 0) must beTrue
			(field.column__ != 0) must beTrue
		}

		"have check false" in {
			val (resultString, result) = field.check
			resultString must be_==(ListBuffer[String]())
			result must beFalse
		}

		"new field" in {
			field.load("src/main/resources/data/default1.ini")
			(field.row__ != 0) must beTrue
			(field.column__ != 0) must beTrue
		}
		
		"generate a String of the form != null" in {
			(field.toString != "") must beTrue
		}
	}
}