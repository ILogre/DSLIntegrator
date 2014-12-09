package fr.unice.i3s.dslintegrator.domains

/**
 * Created by ivan on 01/12/2014.
 * Abstract representation of the object storage and querying,
 * representing the current state of the system.
 */

/*trait History {
  type myMap[+T] = HashMap[String,T]
  var models : myMap[ModelState] = new myMap
}*/

trait Model {
  val name: String
  val version: List[Domain]

  def canEqual(other: Any): Boolean = other.isInstanceOf[Model]

  override def equals(other: Any): Boolean = other match {
    case that: Model =>
      that.name.equals(this.name)
    case _ => false
  }
}