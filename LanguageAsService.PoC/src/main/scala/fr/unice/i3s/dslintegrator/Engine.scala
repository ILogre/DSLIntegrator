package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DashboardDesign, addVisu, addData}
import fr.unice.i3s.dslintegrator.domains.datacenter.mm.Catalog
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{DataCenter, DBModel, addResource}

import scala.collection.mutable._

object Engine {


    def apply(done : Message) : Answer = done match {
      case c  : addData => {
        val dashboard = c.target.version.head.asInstanceOf[Dashboard]
        val newDashboard = DashboardDesign.addData(c).version.head
        if ( Engine(new hasLinked(c.target)).answer.asInstanceOf[Boolean]) {
          val database = Engine( new getLinked(c.target)).answer.asInstanceOf[getLinkedAnswer].a.asInstanceOf[DBModel].version.head
          if (!database.isDefined(c.uri)) database.notify("Ressource needed : " + c.uri)
          else newDashboard.log("Link between "+ c.uri + " and " + c.visuName + " : OK")
        }
        else newDashboard.log("Data "+ c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !")}
        EmptyAnswer //todo personalize the return type

      case c  : addVisu => {
        val newDashboard = DashboardDesign.addVisu(c).version.head
        newDashboard.log("Visu "+ c.visuName +" declaration : OK" )
        EmptyAnswer //todo personalize the return type
      }


      case c  : addResource =>
        val newDashboard =  DataCenter.addResource(c).version.head
        newDashboard.log("Resource "+ c.uri +" declaration : OK" )
        EmptyAnswer //todo personalize the return type

      case other => throw new Exception("Unhandled operation")
    }
  
}




