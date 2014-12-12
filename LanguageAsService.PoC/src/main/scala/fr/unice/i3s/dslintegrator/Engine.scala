package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.service._
import fr.unice.i3s.dslintegrator.domains.datacenter.service._
import fr.unice.i3s.dslintegrator._


object Engine {


    def apply(done : Message) : Answer = done match {
      case c  : addData => {
        DashboardDesign.addData(c)
        if (Engine(new hasLinked(c.dashboardName)).answer.asInstanceOf[Boolean]) {
          val modelLinked = Association.getLinked(new getLinked(c.dashboardName)).answer
          if (!DataCenter.isDefined(new isDefined(modelLinked,c.uri)).answer) {
            Engine(new updateLog(c.dashboardName,"Data " + c.uri + " for " + c.visuName + " : OK, but no related resource detected in associated catalog \n ---> Local coherency but models are not integrated !"))
            Engine(new updateLog(modelLinked,"Ressource needed : " + c.uri +"\n---> Local coherency but models are not integrated !"))
          }
          else {
            Engine(new updateLog(c.dashboardName,"Link between " + c.uri + " and " + c.visuName + " : OK"))
          }
        }
        else {
          Engine(new updateLog(c.dashboardName,"Data " + c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !"))
        }
        EmptyAnswer
      }

      case c  : addVisu => {
        DashboardDesign addVisu c
        Engine(new updateLog(c.dashboardName,"Visu "+ c.visuName +" declaration : OK" ))
        EmptyAnswer
      }


      case c  : addResource =>
        DataCenter addResource c
        Engine(new updateLog(c.catalogName,"Resource "+ c.uri +" declaration : OK" ))
        EmptyAnswer

      case c : isDefined =>
        DataCenter isDefined c

      case c : link =>
        Association link c

      case c : unlink =>
        Association unlink c

      case c : hasLinked =>
        Association hasLinked c

      case c : getLinked =>
        Association getLinked c

      case c : declareDatabase =>
        DataCenter declareDatabase c
        EmptyAnswer

      case c : declareDashboard =>
        DashboardDesign declareDashboard c
        EmptyAnswer

      case c : updateLog =>
        Log.updateLog(c)

      case c : getLog =>
        Log getLog c

      case other => throw new Exception("Unhandled operation")
    }
  
}




