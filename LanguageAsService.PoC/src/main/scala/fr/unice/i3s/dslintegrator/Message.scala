package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.Model

/**
 * Created by ivan on 02/12/2014.
 */
trait Message

trait Answer extends Message {
  val answer : Any
}

object EmptyAnswer extends Answer {
  override val answer: Any = null
}