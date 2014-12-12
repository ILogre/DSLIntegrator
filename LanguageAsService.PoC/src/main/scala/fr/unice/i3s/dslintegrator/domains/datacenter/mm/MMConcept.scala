package fr.unice.i3s.dslintegrator.domains.datacenter.mm

import fr.unice.i3s.dslintegrator.domains.{Domain, MMConcept}

class Catalog(name: String, val ress: List[Resource] = List(),override val log:String = "\n",override val notification:String = "\n") extends MMConcept with Domain {
  def addResource(uri: String, semantic: String, elements: (String, String)*) : Catalog = {
    val elems = elements.map( p => new Element(p._1,p._2)).toList
    new Catalog(this.name, new Resource(uri,semantic, elems) :: this.ress, this.log, this.notification)
  }
  def isDefined(uri : String) : Boolean = !this.ress.filter(r => r.uri == uri ).isEmpty

  override def toString() = "\nCatalog " + this.name + " { \n\tResources "+ress+"\n}"+log+notification

  override def notify(s: String): Catalog = new Catalog(name, ress, log, notification+"\n"+s)

  override def log(s: String): Catalog = new Catalog(name, ress, log+"\n"+s, notification)
}
class Resource(val uri: String, semantic: String, elements: List[Element]) extends MMConcept{
  override def toString() = "\n\t\tres " + this.uri + " means " + this.semantic + " { \n\t\tElements "+this.elements+"\n\t\t}\n\t"
}
class Element(field: String, kind: String) extends MMConcept{
  override def toString() = "\n\t\t\t{ "+this.field+" , "+this.kind+" } "
}