package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.{Domain, Model}
import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.compovisu.service._
import fr.unice.i3s.dslintegrator.domains.datacenter.service._


object Engine {


    def apply(done : Message) : Answer = done match {
      case c  : addData => {
        val dashboard = c.target.version.head.asInstanceOf[Dashboard]
        val newDashboard = DashboardDesign.addData(c).version.head
        if (Engine(new hasLinked(c.target)).answer.asInstanceOf[Boolean]) {
          val modelLinked = Engine(new getLinked(c.target)).answer.asInstanceOf[DBModel]
          val db = DBPersistence
          val database = modelLinked.version.head
          if (!database.isDefined(c.uri)) {
            persist(c.target,newDashboard.log("Data " + c.uri + " for " + c.visuName + " : OK, but no related resource detected in associated catalog \n ---> Local coherency but models are not integrated !"))
            persist(modelLinked,database.notify("Ressource needed : " + c.uri +"\n---> Local coherency but models are not integrated !"))
          }
          else {
            persist(c.target,newDashboard.log("Link between " + c.uri + " and " + c.visuName + " : OK"))
          }
        }
        else {
          persist(c.target,newDashboard.log("Data " + c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !"))
        }
        EmptyAnswer //todo personalize the return type
      }

      case c  : addVisu => {
        val newDashboard = DashboardDesign.addVisu(c).version.head
        persist(c.target,newDashboard.log("Visu "+ c.visuName +" declaration : OK" ))
        EmptyAnswer //todo personalize the return type
      }


      case c  : addResource =>
        val newDataCenter =  DataCenter.addResource(c).version.head
        persist(c.target,newDataCenter.log("Resource "+ c.uri +" declaration : OK" ))
        EmptyAnswer //todo personalize the return type

      case c : link =>
        Association.link(c)

      case c : unlink =>
        Association.unlink(c)

      case c : hasLinked =>
        Association.hasLinked(c)

      case c : getLinked =>
        Association.getLinked(c)

      case c : declareDatabase =>
        DataCenter.declareDatabase(c)
        EmptyAnswer //todo personalize the return type

      case c : declareDashboard =>
        DashboardDesign.declareDashboard(c)
        EmptyAnswer //todo personalize the return type

      case other => throw new Exception("Unhandled operation")
    }

  private def persist(oldModel :Model, newDomain : Domain) = {
    val newModel = oldModel.updateLast(newDomain)
    if ( Engine(new hasLinked(oldModel)).answer.asInstanceOf[Boolean]){
      val linked = Engine((new getLinked(oldModel))).answer.asInstanceOf[Model]
      Engine( new unlink(oldModel, linked))
      Engine(new link(newModel,linked))
    }
  }
  
}




