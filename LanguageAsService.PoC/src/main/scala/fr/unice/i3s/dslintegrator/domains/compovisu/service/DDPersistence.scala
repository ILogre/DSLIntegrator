package fr.unice.i3s.dslintegrator.domains.compovisu.service

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.{Domain, Model}
import scala.collection.immutable.HashMap

/**
 * Created by ivan on 02/12/2014.
 */
object DDPersistence{
  var models = new HashMap[String,DDModel]()

  def updateModel(m : DDModel) ={
    if (models contains m.name)
      models = models - m.name
    models = models + ((m.name,m))
  }


}

class DDModel(val name: String,val version: List[Dashboard] ) extends Model { // todo : version
  DDPersistence updateModel this
  def this(name:String) = {
    this(name,List(new Dashboard(name)))
  }

  override def updateLast(m: Domain): DDModel = {
    if (m.isInstanceOf[Dashboard])
      new DDModel(name,m.asInstanceOf[Dashboard]::version.drop(0))
    else throw new Exception //todo
  }

  override def append(m: Domain): DDModel = {
    if (m.isInstanceOf[Dashboard])
      new DDModel(name,m.asInstanceOf[Dashboard]::version)
    else throw new Exception //todo
  }
}

