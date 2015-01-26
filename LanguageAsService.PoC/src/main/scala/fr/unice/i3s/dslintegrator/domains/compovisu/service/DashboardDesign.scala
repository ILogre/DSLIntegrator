package fr.unice.i3s.dslintegrator.domains.compovisu.service

import fr.unice.i3s.dslintegrator.domains.compovisu.mm._
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{UnknownData, UnknownVisualization}
import fr.unice.i3s.dslintegrator.{Operation, Service, Message}

/**
 * Created by ivan on 25/11/2014.
 * A service is a wrapping around a language.
 * It exposes a subset of feature which can be called from outside, here :
 *  addVisu
 *  addData
 */
 class DashboardDesign extends Service {

  // Allows one to initialize a Dashboard instance
  // one needs to specify its name
  //it's by default empty of visualization
  val declareDashboard = new Function1[declareDashboard,DDModel] with Operation {
    override def apply(v1: declareDashboard): DDModel =
      new DDModel(v1.name)
  }

  // Allows one to add a new visualization to a dashboard, specifying its name and concerns
  val addVisu = new Function1[addVisu, DDModel] with Operation{
    override def apply(v1: addVisu): DDModel = {
      val model = DDPersistence.models.get(v1.dashboardName).get
      val lastDashboard = model.version.head
      if (v1.concerns.isEmpty)
        DDPersistence updateModel model.append(lastDashboard.addVisu(v1.visuName))
      else {
        val concernList = v1.concerns map {case (s,m) => concernFactoryHelper(s,m)}
        DDPersistence updateModel model.append(lastDashboard.addVisu(v1.visuName,concernList.toList: _*))
      }
    }
  }

 // Allows one to link a data to an existing visualization,
 // specifying its unique uri and concerns
  val addData = new Function1[addData, DDModel] with Operation{
    override def apply(v1: addData): DDModel = {
      val model = DDPersistence.models.get(v1.dashboardName).get
      val lastDashboard = model.version.head
      val visu = lastDashboard.getVisuByName(v1.visuName)
      val newVisu = visu.addData(v1.uri)
      if (v1.concerns.isEmpty)
        DDPersistence updateModel model.append(lastDashboard.removeVisu(visu).addVisu(visu.addData(v1.uri)))
      else {
        val concernList = v1.concerns map {case (s,m) => concernFactoryHelper(s,m)}
        val newVisu = visu.addData(v1.uri,concernList.toList: _*)
        val newDashboard = lastDashboard.removeVisu(visu).addVisu(newVisu)
        DDPersistence updateModel model.append(newDashboard)
      }
    }
  }

  // Allows one to add a concern on a visualization
  val addConcernToVisu = new Function1[addConcernToVisu, DDModel] with Operation{
    override def apply(v1: addConcernToVisu): DDModel = {
      val model = DDPersistence.models.get(v1.dashboardName).get
      val lastDashboard = model.version.head

      val visu = lastDashboard.getVisuByName(v1.visuName)

      if (visu!=null){
        val newVisu = visu.addConcern(concernFactoryHelper(v1.concernName, v1.params))
        val newDashboard = lastDashboard.removeVisu(visu).addVisu(newVisu)
        DDPersistence updateModel model.append(newDashboard)
      }
      else throw new UnknownVisualization("Visualization "+v1.visuName+" is unknown within the dashboard "+v1.dashboardName)
    }
  }

  // Allows one to add a concern on a data
  val addConcernToData = new Function1[addConcernToData, DDModel] with Operation{
    override def apply(v1: addConcernToData): DDModel = {
      val model = DDPersistence.models.get(v1.dashboardName).get
      val lastDashboard = model.version.head
      val visu = lastDashboard.getVisuByName(v1.visuName)
      if (visu!=null){
        val data = lastDashboard.visus.flatMap(v => v.data).filter(d => d.uri.equals(v1.dataName)).head
        if (data!=null){
          val newData = data.addConcern(concernFactoryHelper(v1.concernName, v1.params))
          val newVisu = visu.removeData(data).addData(newData)
          val newDashboard = lastDashboard.removeVisu(visu).addVisu(newVisu)
          DDPersistence updateModel model.append(newDashboard)
        }
        else  throw new UnknownData("No data "+v1.dataName+" is linked to visualization "+v1.visuName+" within the dashboard "+v1.dashboardName)
      }
      else throw new UnknownVisualization("Visualization "+v1.visuName+" is unknown within the dashboard "+v1.dashboardName)
    }
  }

  def concernFactoryHelper(concernName : String, params: Map[String,Any] = Map()): Concern = concernName match {
    case "1D" => OneDim
    case "2D" => TwoDim
    case "Proportion" => Proportion
    case "Value" => Value
    case "Threshold" =>
      new Threshold(params("limitmin").asInstanceOf[Double], params("limitmax").asInstanceOf[Double])
  }
}

object DashboardDesign extends DashboardDesign


// Exposed

case class declareDashboard(name : String) extends Message
case class addVisu(dashboardName : String, visuName: String, concerns :Map[String, Map[String,Any]]= Map()) extends Message
case class addData(dashboardName : String, visuName: String, uri: String, concerns :Map[String, Map[String,Any]]= Map()) extends Message
case class addConcernToVisu(dashboardName : String, visuName: String, concernName:String, params: Map[String,Any] = Map()) extends Message
case class addConcernToData(dashboardName : String, visuName: String, dataName: String, concernName:String, params: Map[String,Any] = Map()) extends Message
