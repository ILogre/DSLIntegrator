package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.service._
import fr.unice.i3s.dslintegrator.domains.datacenter.service._
import fr.unice.i3s.dslintegrator._


object Engine {

    def apply(done : Message) : Answer = done match {
      case c  : addData => BusinessRules addDataRule c

      case c  : addVisu => BusinessRules.addVisuRule(c)

      case c  : addResource => BusinessRules.addResourceRule(c)

      case c  : addConcernToVisu => BusinessRules addConcernToVisuRule c

      case c : isDefined => DataCenter isDefined c

      case c : link => Association link c

      case c : unlink => Association unlink c

      case c : hasLinked => Association hasLinked c

      case c : getLinked => Association getLinked c

      case c : declareDatabase => DataCenter declareDatabase c; EmptyAnswer

      case c : declareDashboard => DashboardDesign declareDashboard c; EmptyAnswer

      case c : updateLog => Log.updateLog(c)

      case c : getLog => Log getLog c

      case other => throw new Exception("Unhandled operation")
    }



}


object BusinessRules {

  def addDataRule(c: addData): Answer = {
    DashboardDesign.addData(c)
    if (Engine(new hasLinked(c.dashboardName)).answer.asInstanceOf[Boolean]) {
      val modelLinked = Association.getLinked(new getLinked(c.dashboardName)).answer
      if (!DataCenter.isDefined(new isDefined(modelLinked, c.uri)).answer) {
        Engine(new updateLog(c.dashboardName, "Data " + c.uri + " for " + c.visuName + " : OK, but no related resource detected in associated catalog \n ---> Local coherency but models are not integrated !\n"))
        Engine(new updateLog(modelLinked, "Ressource needed : " + c.uri + "\n---> Local coherency but models are not integrated !\n"))
      }
      else {
        Engine(new updateLog(c.dashboardName, "Link between " + c.uri + " and " + c.visuName + " : OK\n"))
      }
    }
    else {
      Engine(new updateLog(c.dashboardName, "Data " + c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !\n"))
    }
    EmptyAnswer
  }

  def addVisuRule(c: addVisu): Answer = {
    DashboardDesign addVisu c
    Engine(new updateLog(c.dashboardName, "Visu " + c.visuName + " declaration : OK\n"))
    EmptyAnswer
  }

  def addResourceRule(c: addResource): Answer = {
    DataCenter addResource c
    Engine(new updateLog(c.catalogName, "Resource " + c.uri + " declaration : OK\n"))
    EmptyAnswer
  }



  def addConcernToVisuRule(c: addConcernToVisu): Answer = {
    DashboardDesign addConcernToVisu c
    if (c.concernName.equals("Threshold")){
      if (Engine(new hasLinked(c.dashboardName)).answer.asInstanceOf[Boolean]) {
        val modelLinked = Association.getLinked(new getLinked(c.dashboardName)).answer
        DataCenter addResource new addResource(modelLinked,"newURI","Fictive resource to match a Threshold need", ("t","numerical"),("v","numerical"))
        Engine(new updateLog(c.dashboardName, "Concern Threshold added to " + c.visuName + " : OK\n Resource catalog "+modelLinked+" edited with a resource template to be satisfied \n ---> No global consistency !\n"))
      }
      else
        Engine(new updateLog(c.dashboardName, "Concern Threshold added to " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !"))
    }
    Engine(new updateLog(c.dashboardName, "Concern "+c.concernName+" added to visualization "+c.visuName))
    EmptyAnswer
  }

  def addConcernToDataRule(c: addConcernToData): Answer = {
    DashboardDesign addConcernToData c
    if (c.concernName.equals("Threshold")){
      if (Engine(new hasLinked(c.dashboardName)).answer.asInstanceOf[Boolean]) {
        val modelLinked = Association.getLinked(new getLinked(c.dashboardName)).answer
        DataCenter addResource new addResource(modelLinked,"newURI","Fictive resource to match a Threshold need", ("t","numerical"),("v","numerical"))
        Engine(new updateLog(c.dashboardName, "Concern Threshold added to " + c.visuName + " : OK\n Resource catalog "+modelLinked+" edited with a resource template to be satisfied \n ---> No global consistency !\n"))
      }
      else
        Engine(new updateLog(c.dashboardName, "Concern Threshold added to " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !\n"))
    }
    Engine(new updateLog(c.dashboardName, "Concern "+c.concernName+" added to data "+c.dataName+" on visualization "+c.visuName))
    EmptyAnswer
  }
  // todo what about extracting the business rules -> don't repeat the same treatment for 4 diff operation if the trigger is the same
}



