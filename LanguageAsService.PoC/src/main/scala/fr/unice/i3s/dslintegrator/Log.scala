package fr.unice.i3s.dslintegrator

import scala.collection.mutable

/**
 * Created by ivan on 12/12/2014.
 */
private object Log extends Service{
  val logs = mutable.HashMap[String,String]()

  val updateLog = new Function[updateLog,logAnswer] {
    override def apply(v1: updateLog): logAnswer = {
      if(logs.contains(v1.modelId))
        logs.update(v1.modelId,"\n"+logs(v1.modelId)+v1.log)
      else
        logs.put(v1.modelId,"\n"+v1.log+"\n")
      new logAnswer(v1.modelId, logs(v1.modelId))
    }
  }

  val getLog = new Function1[getLog,logAnswer] {
    override def apply(v1: getLog): logAnswer = {
      new logAnswer(v1.modelId, logs(v1.modelId))
    }
  }
}

case class updateLog(modelId: String, log:String) extends Message
case class getLog(modelId: String) extends Message
case class logAnswer(modelId: String, override val answer: String ) extends Answer
