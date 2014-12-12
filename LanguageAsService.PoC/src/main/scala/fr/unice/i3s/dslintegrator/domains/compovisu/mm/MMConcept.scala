package fr.unice.i3s.dslintegrator.domains.compovisu.mm

import fr.unice.i3s.dslintegrator.domains.{Domain, MMConcept}

class Dashboard(val name: String, val visus: List[Visualization] = List()) extends MMConcept with Domain{

  def addVisu(visuName: String, concerns: String*): Dashboard = {
    new Dashboard(this.name, new Visualization(visuName, concerns.toList)::this.visus)}
  def addVisu(visu: Visualization): Dashboard = {
    new Dashboard(this.name, visu::this.visus)
  }
  def removeVisu(visu: Visualization): Dashboard = {
    new Dashboard(this.name, this.visus.drop(this.visus.indexOf(visu)))
  }
  def getVisuByName(name : String): Visualization = {
    def iterGetVisuByName(name : String, visus : List[Visualization] ) : Visualization = {
      if (visus isEmpty)
        throw new Exception
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

class Visualization(val name: String, val concerns: List[String], val data: List[Data] = List()) extends MMConcept {

  override def toString() = "\n\tVisu " + this.name + " { \n\t\t Concerns "+concerns+"\n\t\t Data "+data+"\n\t}"

  def addData(uri: String, concerns: String*): Visualization = new Visualization(this.name, this.concerns, new Data(uri,concerns.toList):: this.data)

}

class Data(val uri: String, val concerns: List[String]) extends MMConcept {
  override def toString() = "\n\tData " + this.uri + " { \n\t\t Concerns " + concerns +"\n\t}"
}