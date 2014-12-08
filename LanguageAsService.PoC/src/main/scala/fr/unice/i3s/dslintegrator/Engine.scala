package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.{Model}
import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DDHistory, DashboardDesign, addVisu, addData}
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{DataCenter, DBModel, addResource}

import scala.collection.mutable._

object Engine {


    def apply(done : Message) = done match {
      case c  : addData => {
        val dashboard = c.target.version.head.asInstanceOf[Dashboard]
        DashboardDesign.addData(c)
        val newDashboard = DDHistory.models.get(c.dashboardName).get.version.head
        if ( Association.hasPair(c.target) ) {
          val database = Association.getLinked(c.target).asInstanceOf[DBModel].version.head
          if (!database.isDefined(c.uri)) database.updateState("Ressource needed : " + c.uri)
          else newDashboard.updateLog("Link between "+ c.uri + " and " + c.visuName + " : OK") }
        else newDashboard.updateLog("Data "+ c.uri + " for " + c.visuName + " : OK, but no association detected with a resource catalog \n ---> No integration possible !")}

      case c  : addVisu => {
        DashboardDesign.addVisu(c.dashboardName,c.visuName,c.concerns:_*) // todo : return the new model to replace c.target
        DDHistory.models.get(c.dashboardName).get.version.head.updateLog("Visu "+ c.visuName +" declaration : OK" )
      }


      case c  : addResource =>
        DataCenter.addResource(c.catalogName,c.uri,c.semantic,c.elements:_*)
        c.target.version.head.updateLog("Resource "+ c.uri +" declaration : OK" )


      case other => throw new Exception("Unhandled operation")
    }
  
}



object Association {
  val asso : MutableList[Pair] = MutableList()

  def link(s1:Model, s2 : Model) ={
    val p:Pair = new Pair(s1,s2)
    if(!known(p))
      asso.+=:(p)
  }

  def hasPair(model : Model):Boolean = {
    def iterGetLinked(m : Model, l:List[Pair]):Boolean = l match {
      case head::tail => head.contains(m) ||  iterGetLinked(m,tail)
      case _ => false
    }
    iterGetLinked(model,asso.toList)
  }

  def getLinked(model : Model):Model = {
    def iterGetLinked(m : Model, l:List[Pair]):Model = l match {
      case head::tail =>
        if (head.contains(m))
          head.getPaired(m)
        else iterGetLinked(m,tail)
      case _ => throw new Exception //todo
    }
    iterGetLinked(model,asso.toList)
  }

  private def known(p:Pair) : Boolean = {
    def iterKnown(p:Pair, l:List[Pair]):Boolean = l match {
      case head::tail => head.equals(p) || iterKnown(p,tail)
      case _ => false
    }
    iterKnown(p,asso.toList)
  }

  class Pair(val model1 : Model,val model2 : Model) {

    def getPaired(model:Model):Model= model match {
      case `model1` => model2
      case `model2` => model1
      case _ => throw new Exception //todo
    }

    def contains(m : Model ):Boolean = m.equals(model1)||m.equals(model2)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Pair]

    override def equals(other: Any): Boolean = other match {
      case that: Pair =>
        (that canEqual this) &&
          (( model1 == that.model1 &&
            model2 == that.model2 ) ||
            ( model1 == that.model2 &&
              model2 == that.model1 ))
      case _ => false
    }
  }

}