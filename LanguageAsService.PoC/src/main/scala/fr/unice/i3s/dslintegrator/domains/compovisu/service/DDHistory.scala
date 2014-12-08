package fr.unice.i3s.dslintegrator.domains.compovisu.service

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import fr.unice.i3s.dslintegrator.domains.{Model}
import scala.collection.immutable.HashMap

/**
 * Created by ivan on 02/12/2014.
 */
object DDHistory{
  var models = new HashMap[String,DDModel]()

  def updateModel(m : DDModel) ={
    if (models contains m.name)
      models = models - m.name
    models = models + ((m.name,m))
  }


}

class DDModel(val name: String,val version: List[Dashboard] ) extends Model { // todo : version
  def this(name:String) = {
    this(name,List(new Dashboard(name)))
    DDHistory updateModel this
  }
}

