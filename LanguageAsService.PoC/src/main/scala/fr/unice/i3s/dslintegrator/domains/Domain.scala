package fr.unice.i3s.dslintegrator.domains

trait Domain {
  val notification : String = "\n" //todo : immutable
  val log : String = "\n"

  def notify(s: String):Domain
  def log(s: String):Domain
}
