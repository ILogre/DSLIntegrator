package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DDHistory, addData, addVisu, DDModel}
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{DBHistory, addResource, DBModel}

/**
 * Created by ivan on 02/12/2014.
 */
object main extends App{
  // define some names
  val dbName = "db"
  val ddName = "dash"

  // creation of a new dashboard and a new database to work on
  val ddhist = new DDModel(ddName)
  val dbhist = new DBModel(dbName)
  Association.link(ddhist,dbhist)
  
  // As a data manager, I define a new resource to be used
  val call = addResource(dbName,"http://users.polytech.unice.fr/~logre/resources/air_temp.senml","Outside temperature in °C",("t","numerical"),("v","numerical"))
  Engine(call)

  // Some trace to verify what's going on
  println("phase 1 ")
  // roles are not suppose to access the history of models, but for debug it's necessary ;)
  val ddUpdated_1 = DDHistory.models.get(ddName).get.version.head
  println(ddUpdated_1)


  // As a dashboard designer, I fill my dashboard with a new visualization and link it with some data
  Engine( addVisu(ddName,"myFirstVisu", "Threshold","2D") )
  Engine( addData(ddName, "myFirstVisu","http://users.polytech.unice.fr/~logre/resources/air_temp.senml","Value") )
  
  // Some trace to verify what's going on
  println("phase 2 ")
  // roles are not suppose to access the history of models, but for debug it's necessary ;)
  val ddUpdated_2 = DDHistory.models.get(ddName).get.version.head
  val dbUpdated_2 = DBHistory.models.get(dbName).get.version.head
  println(ddUpdated_2)
  println(dbUpdated_2)
}
