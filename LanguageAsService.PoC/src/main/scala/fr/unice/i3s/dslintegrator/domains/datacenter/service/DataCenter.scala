package fr.unice.i3s.dslintegrator.domains.datacenter.service

import fr.unice.i3s.dslintegrator.{Service, Message}
import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.compovisu.service.{DDModel, DDPersistence}
import fr.unice.i3s.dslintegrator.domains.Model

/**
 * Created by ivan on 25/11/2014.
 * A service is a wrapping around a language.
 * It exposes a subset of feature which can be called from outside, here :
 *  addResource
 *  isDefined
 */
class DataCenter extends Service{

  // Allows one to add a new resource to a catalog
  // specifying its unique uri, its semantic and of which element it is composed
  def addResource(catalogName : String, uri: String, semantic: String, elements: (String, String)*) = {
    val model = DBPersistence.models.get(catalogName).get
    val lastCatalog = model.version.head
    val newCatalog = lastCatalog.addResource(uri, semantic, elements : _*)
    val newListCatalog = newCatalog :: model.version
    DBPersistence addModel new DBModel(model.name,newListCatalog)

  }

  // Allows one to check the existence of a given resource in a catalog
  def isDefined(catalogName : String, uri : String) : Boolean = {
    val model = DBPersistence.models.get(catalogName).get
    val lastCatalog = model.version.head
    lastCatalog.isDefined(uri)
  }

}

object DataCenter extends DataCenter


// Exposed

case class addResource(catalogName : String, uri: String, semantic: String, elements: (String, String)*) extends Message{
  override val target: Model = DBPersistence.models.get(catalogName).get
}
