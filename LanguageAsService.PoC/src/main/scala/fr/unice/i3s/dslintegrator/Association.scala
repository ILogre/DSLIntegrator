package fr.unice.i3s.dslintegrator

import fr.unice.i3s.dslintegrator.domains.compovisu.mm.Dashboard
import scala.collection.mutable.ListBuffer

/**
 * Created by ivan on 09/12/2014.
 */
private object Association extends Service{
  val asso : ListBuffer[Pair] = ListBuffer()

  val link = new Function1[link, linkAnswer] with Operation {
    override def apply(v1: link): linkAnswer = {
      if ( ! ( v1.s1.isInstanceOf[Dashboard] & v1.s2.isInstanceOf[Dashboard])  ) {
        val p: Pair = new Pair(v1.s1, v1.s2)
        if (!known(p))
          Association.asso.+=:(p)
        new linkAnswer(v1.s1, v1.s2, true)
      }
      else
        new linkAnswer(v1.s1,v1.s2,false)
    }
  }

  val unlink = new Function1[unlink, unlinkAnswer] with Operation {
    override def apply(v1: unlink): unlinkAnswer = {
      val p: Pair = new Pair(v1.s1, v1.s2)
      if (known(p)){
        Association.asso.remove(Association.asso.indexOf(p))
        new unlinkAnswer(v1.s1, v1.s2, true)
      }
      else new unlinkAnswer(v1.s1, v1.s2, false)

    }
  }

  val hasLinked =  new Function1[hasLinked, hasLinkedAnswer] with Operation {
    override def apply(v1: hasLinked): hasLinkedAnswer = {
      def iterGetLinked(m : String, l:List[Pair]):Boolean = l match {
        case head::tail => head.contains(m) ||  iterGetLinked(m,tail)
        case _ => false
      }
      val bool = iterGetLinked(v1.s,Association.asso.toList)
      new hasLinkedAnswer(v1.s, bool )
    }
  }

  val getLinked = new Function1[getLinked,getLinkedAnswer] {
    override def apply(v1: getLinked): getLinkedAnswer = {
      def iterGetLinked(m : String, l:List[Pair]):String = l match {
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

class Pair(val id1 : String,val id2 : String) {

  def getPaired(id:String):String= id match {
    case `id1` => id2
    case `id2` => id1
    case _ => throw new Exception //todo
  }

  def contains(s : String ):Boolean = s.equals(id1)||s.equals(id2)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Pair]

  override def equals(other: Any): Boolean = other match {
    case that: Pair =>
      (that canEqual this) &&
        (( id1 == that.id1 &&
          id2 == that.id2 ) ||
          ( id1 == that.id2 &&
            id2 == that.id1 ))
    case _ => false
  }
}

case class link(s1:String, s2 : String) extends Message
case class linkAnswer(s1:String, s2 : String, a:Boolean) extends Answer{
  override val answer: Boolean = a
}

case class unlink(s1:String, s2 : String) extends Message
case class unlinkAnswer(s1:String, s2 : String, a:Boolean) extends Answer{
  override val answer: Boolean = a
}

case class hasLinked(s:String) extends Message
case class hasLinkedAnswer(s:String,a:Boolean) extends Answer {
  override val answer: Boolean = a
}

case class getLinked(s:String) extends Message
case class getLinkedAnswer(s:String,a:String) extends Answer {
  override val answer: String = a
}