package fr.unice.i3s.dslintegrator.domains.compovisu.mm

import fr.unice.i3s.dslintegrator.domains.{Domain, MMConcept}

class Dashboard(val name: String, val visus: List[Visualization] = List()) extends MMConcept with Domain{

  def addVisu(visuName: String, concerns: Concern*): Dashboard = {
    new Dashboard(this.name, new Visualization(visuName, concerns.toList)::this.visus)}
  def addVisu(visu: Visualization): Dashboard = {
    new Dashboard(this.name, visu::this.visus)
  }
  def removeVisu(visu: Visualization): Dashboard = {
    if(this.visus.contains(visu))
      new Dashboard(this.name, this.visus.filter(v => !v.equals(visu)))
    else
      this
  }
  def getVisuByName(name : String): Visualization = {
    def iterGetVisuByName(name : String, visus : List[Visualization] ) : Visualization = {
      if (visus isEmpty)
        throw null
      else
        if (visus.head.name.toString()==name) visus.head
        else iterGetVisuByName(name,visus.tail)
    }
    iterGetVisuByName(name,this.visus)
  }

  override def toString() = {
    "\nDashboard " + this.name + " { \n\tVisualizations "+visus+"\n}"
  }
}

class Visualization(val name: String, val concerns: List[Concern], val data: List[Data] = List()) extends MMConcept with Concerned {

  override def toString() = "\n\tVisu " + this.name + " { \n\t\t Concerns "+concerns+"\n\t\t Data \n\t\t\t"+data+"\n\t}"

  //def addData(uri: String): Visualization = new Visualization(this.name, this.concerns, new Data(uri):: this.data)
  def addData(data :Data): Visualization = new Visualization(this.name, this.concerns, data::this.data)
  def addData(uri: String, concerns: Concern*): Visualization = new Visualization(this.name, this.concerns, new Data(uri,concerns.toList):: this.data)
  def removeData(data: Data): Visualization= new Visualization(this.name, this.concerns, this.data.drop(this.data.indexOf(data)))

  override def addConcern(c: Concern): Visualization = new Visualization(this.name, c::this.concerns, this.data)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Visualization]

  override def equals(other: Any): Boolean = other match {
    case that: Visualization =>
      (that canEqual this) &&
        name == that.name &&
        concerns == that.concerns &&
        data == that.data
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, concerns, data)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Data(val uri: String, val concerns: List[Concern]) extends MMConcept with Concerned {
  def this(uri: String) = this(uri,List())
  override def toString() = "Data " + this.uri + " { \n\t\t\t\t Concerns \n\t\t\t\t\t" + concerns +"\n\t\t\t}"

  override def addConcern(c: Concern): Data = new Data(this.uri,c::this.concerns)
}

abstract class Concern(val name : String) extends MMConcept {
  override def toString() = name+" "
}

class Threshold(val limitmin : Double, val limitmax: Double) extends Concern("Threshold") {}
object OneDim extends Concern("1D")
object TwoDim extends Concern("2D")
object Proportion extends Concern("Proportion")
object Value extends Concern("Value")

trait Concerned {
  val concerns : List[Concern]
  def addConcern(c : Concern): Concerned
}
