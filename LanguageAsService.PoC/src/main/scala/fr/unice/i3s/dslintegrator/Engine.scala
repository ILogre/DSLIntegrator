package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DashboardDesign, addVisu, addData}
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{DataCenter, DBModel, addResource}


object Engine {


    def apply(done : Message) : Answer = done match {
      case c  : addData => {
        val dashboard = c.target.version.head.asInstanceOf[Dashboard]
        val newDashboard = DashboardDesign.addData(c).version.head
        if (Engine(new hasLinked(c.target)).answer.asInstanceOf[Boolean]) {
          val modelLinked = Engine(new getLinked(c.target)).answer.asInstanceOf[getLinkedAnswer].a.asInstanceOf[DBModel]
          val database = modelLinked.version.head
          if (!database.isDefined(c.uri))
            modelLinked.updateLast(database.notify("Ressource needed : " + c.uri))
          else {
            c.target.updateLast(newDashboard.log("Link between " + c.uri + " and " + c.visuName + " : OK"))

          }
        }
        else {
          c.target.updateLast(newDashboard.log("Data " + c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !"))
        }
        EmptyAnswer //todo personalize the return type
      }

      case c  : addVisu => {
        val newDashboard = DashboardDesign.addVisu(c).version.head
         c.target.updateLast( newDashboard.log("Visu "+ c.visuName +" declaration : OK" ))
        EmptyAnswer //todo personalize the return type
      }


      case c  : addResource =>
        val newDashboard =  DataCenter.addResource(c).version.head
        newDashboard.log("Resource "+ c.uri +" declaration : OK" )
        EmptyAnswer //todo personalize the return type

      case c : link =>
        Association.link(c)

      case c : hasLinked =>
        Association.hasLinked(c)

      case c : getLinked =>
        Association.getLinked(c)

      case other => throw new Exception("Unhandled operation")
    }
  
}




