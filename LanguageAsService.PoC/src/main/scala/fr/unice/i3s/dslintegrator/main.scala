package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DDPersistence, DDModel, addData, addVisu}
import fr.unice.i3s.dslintegrator.domains.datacenter.service.{DBPersistence, DBModel, addResource}

/**
 * Created by ivan on 02/12/2014.
 */
object main extends App{

  // define some names
  val dbName = "db"
  val ddName = "dash"

  // creation of a new dashboard and a new database to work on
  val dashboardDesignModel = new DDModel(ddName)
  val databaseModel = new DBModel(dbName)
  Engine(link(dashboardDesignModel,databaseModel))

  // work on the models
  declareResource_Sample()
  //declareVisualizationAndLinkExistingData_Sample()
  declareVisualizationAndLinkUnknownData_Sample()


  def declareResource_Sample() = {

    // As a data manager, I define a new resource to be used
    val call = addResource(dbName,"http://users.polytech.unice.fr/~logre/resources/air_temp.senml","Outside temperature in °C",("t","numerical"),("v","numerical"))
    Engine(call)

    // Some trace to verify what's going on
    println("Function declareResource_Sample :")
    // roles are not suppose to access the history of models, but for debug it's necessary ;)
    val dbUpdated_1 = DBPersistence.models.get(dbName).get.version.head
    println(dbUpdated_1)

  }

  def declareVisualizationAndLinkExistingData_Sample() = {
    // As a dashboard designer, I fill my dashboard with a new visualization and link it with some data
    Engine( addVisu(ddName,"myFirstVisu", "Threshold","2D") )
    Engine( addData(ddName, "myFirstVisu","http://users.polytech.unice.fr/~logre/resources/air_temp.senml","Value") )

    // Some trace to verify what's going on
    println("Function declareVisualizationAndLinkExistingData_Sample")
    // roles are not suppose to access the history of models, but for debug it's necessary ;)
    val ddUpdated_2 = DDPersistence.models.get(ddName).get.version.head
    val dbUpdated_2 = DBPersistence.models.get(dbName).get.version.head
    println(ddUpdated_2)
    println(dbUpdated_2)
  }

  def declareVisualizationAndLinkUnknownData_Sample() = {
    // As a dashboard designer, I fill my dashboard with a new visualization and link it with some data
    Engine( addVisu(ddName,"myFirstVisu", "Threshold","2D") )
    Engine( addData(ddName, "myFirstVisu","http://opendata.paris.fr/api/records/1.0/search?dataset=les-surfaces-boisees","Value") )

    // Some trace to verify what's going on
    println("Function declareVisualizationAndLinkUnknownData_Sample")
    // roles are not suppose to access the history of models, but for debug it's necessary ;)
    val ddUpdated_2 = DDPersistence.models.get(ddName).get.version.head
    val dbUpdated_2 = DBPersistence.models.get(dbName).get.version.head
    println(ddUpdated_2)
    println(dbUpdated_2)
  }

}


