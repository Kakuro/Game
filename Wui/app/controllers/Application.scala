package controllers

import play.api.data.Form
import play.api.data.Forms.nonEmptyText
import play.api.mvc.Action
import play.api.mvc.Controller
import scala.swing.FileChooser
import java.io.File
import javax.swing.filechooser.FileNameExtensionFilter
import java.awt.Frame
import scala.swing.Dialog
import play.Play

object Application extends Controller {

	val controller = new controllers.KakuroWuiController
	val filePath = "app/data/"
		
	
	val commandForm = Form("value" -> nonEmptyText)

	def index = Action {
		Ok(views.html.index(commandForm, controller))
	}

	def loadFile = Action(parse.multipartFormData) { request =>
		request.body.file("filename").map { filename =>
			println("command received : " + filePath + filename)
			controller.level(filePath + filename.filename)
			Ok(views.html.index(commandForm, controller))
		}.getOrElse {
			Redirect(routes.Application.index).flashing(
				"error" -> "Missing file")
		}
	}
	
	def setValue(row: Int, column: Int) = Action { implicit request =>
		{
			commandForm.bindFromRequest.fold(
				formWithErrors => BadRequest(views.html.index(commandForm, controller)),
				{
					case (value) => {
						println("command received : " + row + " " + column + " " + value)
						try {
							controller.setValue(row, column, value.toInt)
						} catch {
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