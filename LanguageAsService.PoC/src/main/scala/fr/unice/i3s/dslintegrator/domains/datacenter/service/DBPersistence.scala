package fr.unice.i3s.dslintegrator.domains.datacenter.service

import fr.unice.i3s.dslintegrator.domains.datacenter.mm.Catalog
import fr.unice.i3s.dslintegrator.domains.{ Model}
import scala.collection.immutable.HashMap

/**
 * Created by ivan on 02/12/2014.
 */
object DBPersistence{
  var models = new HashMap[String,DBModel]()

  def addModel(m : DBModel) ={
    if (models contains m.name)
      models = models - m.name
    models = models + ((m.name,m))
  }
}

class DBModel(val name: String, val version: List[Catalog]) extends Model {
  def this(name:String) = {
    this(name,List(new Catalog(name)))
    DBPersistence addModel this
  }
}