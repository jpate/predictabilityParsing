package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

class DMVGrammar( vocabulary:Set[ObservedLabel] ) {

  val p_order = new LogCPT( vocabulary + Root, dmv.attachmentOrder )

  private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
  val p_stop = new LogCPT( p_stop_keys, dmv.stopDecision )

  private val p_choose_keys = dmv.chooseKeys( vocabulary )
  val p_choose = new LogCPT( p_choose_keys, vocabulary+Root )

  def setParams( otherGram:DMVGrammar ) {
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
  }

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

}

