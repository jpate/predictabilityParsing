package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

class DMVGrammar( vocabulary:Iterable[ObservedLabel] ) {

  val p_order = new LogCPT( vocabulary, dmv.attachmentOrder )

  private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
  val p_stop = new LogCPT( p_stop_keys, stopAttaching )

  private val p_choose_keys = dmv.chooseKeys( vocabulary )
  val p_choose = new LogCPT( p_choose_keys, w )

  def setParams( otherGram:DMVGrammar ) {
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
  }

  def setParams(
    otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
    otherP_stop:LogCPT[StopAttaching,StopOrNot]
    otherP_choose:LogCPT[ObservedLabel,ChooseArgument]
  ) {
    p_order.setCPT( otherP_order.getCPT )
    p_stop.setCPT( otherP_stop.getCPT )
    p_choose.setCPT( otherP_choose.getCPT )
  }

}

