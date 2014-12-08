package fr.unice.i3s.dslintegrator.domains.compovisu.service

import fr.unice.i3s.dslintegrator.{Operation, Service, Message}
import fr.unice.i3s.dslintegrator.domains.Model

/**
 * Created by ivan on 25/11/2014.
 * A service is a wrapping around a language.
 * It exposes a subset of feature which can be called from outside, here :
 *  addVisu
 *  addData
 */
 class DashboardDesign extends Service {
  // Allows one to add a new visualization to a dashboard, specifying its name and concerns
  def addVisu(dashboardName: String, visuName: String, concerns: String*) = {
    val model = DDHistory.models.get(dashboardName).get
    val lastDashboard = model.version.head

    DDHistory updateModel new DDModel(model.name, lastDashboard.addVisu(visuName, concerns: _*) :: model.version)
  }

  // Allows one to link a data to an existing visualization,
  // specifying its unique uri and concerns
 /* def addData(dashboardName: String, visuName: String, uri: String, concerns: String*) = {
    val model = DDHistory.models.get(dashboardName).get
    val lastDashboard = model.states.head
    val visu = lastDashboard.getVisuByName(visuName)
    val newVisu = visu.addData(uri, concerns: _*)
    val newDashboard = lastDashboard.removeVisu(visu).addVisu(newVisu)

    DDHistory addModel new DDModel(model.name, newDashboard :: model.states)
  }*/

  val addData = new Function1[addData, DDModel] with Operation{
    override def apply(v1: addData): Model = {
      val model = DDHistory.models.get(v1.dashboardName).get
      val lastDashboard = model.version.head
      val visu = lastDashboard.getVisuByName(v1.visuName)
      val newVisu = visu.addData(v1.uri, v1.concerns: _*)
      val newDashboard = lastDashboard.removeVisu(visu).addVisu(newVisu)
      val newVersion = new DDModel(model.name, newDashboard :: model.version)
      DDHistory updateModel newVersion
      newVersion
    }
  }
}

object DashboardDesign extends DashboardDesign


// Exposed

case class addVisu(dashboardName : String, visuName: String, concerns: String*) extends Message {
  override val target: Model = DDHistory.models.get(dashboardName).get
}
case class addData(dashboardName : String, visuName: String, uri: String, concerns: String*) extends Message{
  override val target: Model = DDHistory.models.get(dashboardName).get
}