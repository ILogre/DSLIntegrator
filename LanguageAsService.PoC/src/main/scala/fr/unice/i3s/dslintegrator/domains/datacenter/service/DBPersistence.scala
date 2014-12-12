package fr.unice.i3s.dslintegrator.domains.datacenter.service

import fr.unice.i3s.dslintegrator.domains.datacenter.mm.Catalog
import fr.unice.i3s.dslintegrator.domains.{Domain, Model}
import scala.collection.immutable.HashMap

/**
 * Created by ivan on 02/12/2014.
 */
object DBPersistence{
  var models = new HashMap[String,DBModel]()

  def updateModel(m : DBModel): DBModel ={
    if (models contains m.name)
      models = models - m.name
    models = models + ((m.name,m))
    m
  }
}

class DBModel(val name: String, val version: List[Catalog]) extends Model {
  DBPersistence updateModel this
  def this(name:String) = {
    this(name,List(new Catalog(name)))
  }

  override def updateLast(m: Domain): DBModel = {
    if (m.isInstanceOf[Catalog])
      new DBModel(name,m.asInstanceOf[Catalog]::version.drop(0))
    else throw new Exception //todo
  }

  override def append(m: Domain): DBModel = {
    if (m.isInstanceOf[Catalog])
      new DBModel(name,m.asInstanceOf[Catalog]::version)
    else throw new Exception //todo
  }
}
