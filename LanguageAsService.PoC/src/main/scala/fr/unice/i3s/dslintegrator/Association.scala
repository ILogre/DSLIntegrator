package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.Model
import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard

import scala.collection.mutable.MutableList

/**
 * Created by ivan on 09/12/2014.
 */
private object Association extends Service{
  val asso : MutableList[Pair] = MutableList()

  val link = new Function1[link, linkAnswer] with Operation {
    override def apply(v1: link): linkAnswer = {
      if ( ! v1.s1.isInstanceOf[Dashboard] & v1.s2.isInstanceOf[Dashboard] ) {
        val p: Pair = new Pair(v1.s1, v1.s2)
        if (!known(p))
          Association.asso.+=:(p)
        new linkAnswer(v1.s1, v1.s2, true)
      }
      else
        new linkAnswer(v1.s1,v1.s2,false)
    }
  }

  val hasLinked =  new Function1[hasLinked, hasLinkedAnswer] with Operation {
    override def apply(v1: hasLinked): hasLinkedAnswer = {
      def iterGetLinked(m : Model, l:List[Pair]):Boolean = l match {
        case head::tail => head.contains(m) ||  iterGetLinked(m,tail)
        case _ => false
      }
      new hasLinkedAnswer(v1.s, iterGetLinked(v1.s,Association.asso.toList) )
    }
  }

  val getLinked = new Function1[getLinked,getLinkedAnswer] {
    override def apply(v1: getLinked): getLinkedAnswer = {
      def iterGetLinked(m : Model, l:List[Pair]):Model = l match {
        case head::tail =>
          if (head.contains(m))
            head.getPaired(m)
          else iterGetLinked(m,tail)
        case _ => throw new Exception //todo
      }
      new getLinkedAnswer(v1.s,iterGetLinked(v1.s,Association.asso.toList))
    }
  }

  private def known(p:Pair) : Boolean = {
    def iterKnown(p:Pair, l:List[Pair]):Boolean = l match {
      case head::tail => head.equals(p) || iterKnown(p,tail)
      case _ => false
    }
    iterKnown(p,Association.asso.toList)
  }
}

class Pair(val model1 : Model,val model2 : Model) {

  def getPaired(model:Model):Model= model match {
    case `model1` => model2
    case `model2` => model1
    case _ => throw new Exception //todo
  }

  def contains(m : Model ):Boolean = m.equals(model1)||m.equals(model2)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Pair]

  override def equals(other: Any): Boolean = other match {
    case that: Pair =>
      (that canEqual this) &&
        (( model1 == that.model1 &&
          model2 == that.model2 ) ||
          ( model1 == that.model2 &&
            model2 == that.model1 ))
    case _ => false
  }
}

case class link(s1:Model, s2 : Model) extends Message
case class linkAnswer(s1:Model, s2 : Model, a:Boolean) extends Answer{
  override val answer: Boolean = a
}

case class hasLinked(s:Model) extends Message
case class hasLinkedAnswer(s:Model,a:Boolean) extends Answer {
  override val answer: Boolean = a
}

case class getLinked(s:Model) extends Message
case class getLinkedAnswer(s:Model,a:Model) extends Answer {
  override val answer: Model = a
}