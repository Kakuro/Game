package controllers

import play.api.data.Form
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.Controller

object Application extends Controller {

	val controller = new controllers.KakuroWuiController

	val commandForm = Form("value" -> nonEmptyText)
	
	def index = Action {
		Ok(views.html.index(commandForm, controller))
	}

	def setValue(row: Int, column: Int) = Action { implicit request =>
		{
			commandForm.bindFromRequest.fold(
				formWithErrors => BadRequest(views.html.index(commandForm, controller)),
				{
					case (value) => {
						println("command received : " + row + " " + column + " " + value)
						try{
							controller.setValue(row, column, value.toInt)
						}
						catch {
							case e: NumberFormatException => println("Error => is not a number.")
						}
						Ok(views.html.index(commandForm, controller))
					}
				})
		}
	}

	def check = Action { implicit request =>
		{
			println("command received : check")
			controller.check
			Ok(views.html.index(commandForm, controller))
		}
	}

	def reset = Action { implicit request =>
		{
			println("command received : reset")
			controller.reset
			Ok(views.html.index(commandForm, controller))
		}
	}
	
	def level(name: String) = Action { implicit request =>
		{
			println("command received : " + name)
			controller.level(name)
			Ok(views.html.index(commandForm, controller))
		}
	}
}