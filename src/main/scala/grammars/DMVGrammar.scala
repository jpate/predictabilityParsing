package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.util.Math

//class DMVGrammar( vocabulary:Set[ObservedLabel] ) {
abstract class AbstractDMVGrammar {//( vocabulary:Set[ObservedLabel] ) {

  //protected val p_order = new LogCPT( vocabulary + Root, dmv.attachmentOrder )
  protected val p_order = new LogCPT( Set[ObservedLabel](), dmv.attachmentOrder )

  //private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
  //protected val p_stop = new LogCPT( p_stop_keys, dmv.stopDecision )
  protected val p_stop = new LogCPT( Set[StopOrNot](), dmv.stopDecision )

  //private val p_choose_keys = dmv.chooseKeys( vocabulary )
  //protected val p_choose = new LogCPT( p_choose_keys, vocabulary+Root )
  protected val p_choose = new LogCPT( Set[ChooseArgument](), Set[ObservedLabel]() )//vocabulary+Root )

  def emptyPartialCounts = new DMVPartialCounts

  def getParams:DMVParameters
  def setParams[P<:DMVParameters]( parameters:P ) /*{
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
  }*/

  def orderScore( word:ObservedLabel, pref:AttachmentOrder ) = p_order( word, pref )
  def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) = p_stop( stopKey, stopDecision )
  def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) = p_choose( chooseKey, arg )

  def normalize {
    p_order.normalize
    p_stop.normalize
    p_choose.normalize
  }

  def setParams(
    otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
    otherP_stop:LogCPT[StopOrNot,StopDecision],
    otherP_choose:LogCPT[ChooseArgument,ObservedLabel]
  ) {
    p_order.setCPT( otherP_order.getCPT )
    p_stop.setCPT( otherP_stop.getCPT )
    p_choose.setCPT( otherP_choose.getCPT )
  }

  override def toString =
    "P_Order:\n" + p_order +
    "P_Stop:\n" + p_stop +
    "P_Choose:\n" + p_choose

}

class DMVGrammar extends AbstractDMVGrammar{
  def setParams[P<:DMVParameters]( parameters:P ) {
    val VanillaDMVParameters( newP_order, newP_stop, newP_choose) = parameters
    p_order.setCPT( newP_order.getCPT )
    p_stop.setCPT( newP_stop.getCPT )
    p_choose.setCPT( newP_choose.getCPT )
  }
  def getParams = VanillaDMVParameters( p_order, p_stop, p_choose )
}

