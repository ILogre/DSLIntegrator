package fr.unice.i3s.dslintegrator.domains.datacenter.service

import fr.unice.i3s.dslintegrator._

/**
 * Created by ivan on 25/11/2014.
 * A service is a wrapping around a language.
 * It exposes a subset of feature which can be called from outside, here :
 *  addResource
 *  isDefined
 */
class DataCenter extends Service {

  // Allows one to initialize a catalog instance
  // one needs to specify its name
  //it's by default empty of resources
  val declareDatabase = new Function1[declareDatabase,DBModel] with Operation {
    override def apply(v1: declareDatabase): DBModel =
      new DBModel(v1.name)
  }

  // Allows one to add a new resource to a catalog
  // specifying its unique uri, its semantic and of which element it is composed
  val addResource = new Function1[addResource, DBModel] with Operation {
    override def apply(v1: addResource): DBModel = {
      val model = DBPersistence.models.get(v1.catalogName).get
      val lastCatalog = model.version.head
      val newCatalog = lastCatalog.addResource(v1.uri, v1.semantic, v1.elements: _*)
      DBPersistence updateModel model.append(newCatalog)
    }
  }

  // Allows one to check the existence of a given resource in a catalog
  val isDefined = new Function1[isDefined, isDefinedAnswer] with Operation {
    override def apply(v1: isDefined): isDefinedAnswer = {
      val model = DBPersistence.models.get(v1.catalogName).get
      val lastCatalog = model.version.head
      isDefinedAnswer(v1.catalogName,v1.uri,lastCatalog.isDefined(v1.uri))
    }
  }
}

object DataCenter extends DataCenter


// Exposed

case class declareDatabase(name : String) extends Message

case class addResource(catalogName : String, uri: String, semantic: String, elements: (String, String)*) extends Message

case class isDefined(catalogName : String, uri: String) extends Message
case class isDefinedAnswer(catalogName : String, uri: String, override val answer: Boolean) extends Answer
