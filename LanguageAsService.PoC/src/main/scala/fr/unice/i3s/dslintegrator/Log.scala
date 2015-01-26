package fr.unice.i3s.dslintegrator

import java.text.SimpleDateFormat
import java.util.Calendar

import scala.collection.mutable

/**
 * Created by ivan on 12/12/2014.
 */
private object Log extends Service{
  val logs = mutable.HashMap[String,String]()

  val updateLog = new Function[updateLog,logAnswer] {
    override def apply(v1: updateLog): logAnswer = {
      if(logs.contains(v1.modelId))
        logs.update(v1.modelId,getFormattedCurrentTime+"\n"+v1.log+"\n"+logs(v1.modelId))
      else
        logs.put(v1.modelId,getFormattedCurrentTime+"\n"+v1.log+"\n")
      new logAnswer(v1.modelId, logs(v1.modelId))
    }
  }

  val getLog = new Function1[getLog,logAnswer] {
    override def apply(v1: getLog): logAnswer = {
      new logAnswer(v1.modelId, logs(v1.modelId))
    }
  }

  def getFormattedCurrentTime():String ={
    val today = Calendar.getInstance().getTime()
    // create the date/time formatters
    val minuteFormat = new SimpleDateFormat("mm")
    val hourFormat = new SimpleDateFormat("hh")
    val secondFormat = new SimpleDateFormat("ss")
    val millisecondFormat = new SimpleDateFormat("SS")
    val amPmFormat = new SimpleDateFormat("a")

    val currentHour = hourFormat.format(today)
    val currentMinute = minuteFormat.format(today)
    val amOrPm = amPmFormat.format(today)
    val currentSecond = secondFormat.format(today)
    val currentMillisecond = millisecondFormat.format(today)

    currentHour+"h"+currentMinute+"\'"+currentSecond+"\""+currentMillisecond+amOrPm
  }
}

case class updateLog(modelId: String, log:String) extends Message
case class getLog(modelId: String) extends Message
case class logAnswer(modelId: String, override val answer: String ) extends Answer
