package fr.unice.i3s.dslintegrator.domains.datacenter.service

/**
 * Created by ivan on 22/01/2015.
 */
abstract class DDException(val message : String) extends Exception{}
class UnknownDashboard(override val message : String) extends DDException(message){}
class UnknownVisualization(override val message : String) extends DDException(message){}
class UnknownData(override val message : String) extends DDException(message){}