package fr.unice.i3s.dslintegrator.domains

trait Domain {
  var notification : String = "\n" //todo : immutable
  var log : String = "\n"

  def updateState(s: String) = notification = notification+"\t"+s+"\n"
  def updateLog(s: String) = log = log+"\t"+s+"\n"
}
