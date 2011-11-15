package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.partialCounts.DMVTwoStreamHeadsPartialCounts
// import predictabilityParsing.types.tables._
// import predictabilityParsing.util.Math

//class DMVTwoStreamHeadsGrammar( vocabulary:Set[ObservedLabel] ) extends DMVGrammar( vocabulary ) {
class DMVTwoStreamHeadsGrammar /*( vocabulary:Set[ObservedLabel] )*/
  extends DMVGrammar /*( vocabulary)*/ {

  println( "Initializing a DMVTwoStreamHeadsGrammar :) " )

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    arg match {
      case WordPair( w1, _ ) => {
        p_choose( chooseKey, Word(w1) )
      }
      case other:ObservedLabel => {
        p_choose( chooseKey, arg )
      }
    }

  override def emptyPartialCounts = new DMVTwoStreamHeadsPartialCounts

}

// class DMVTwoStreamHeads( vocabulary:Set[ObservedLabelPair] ) {
// 
//   //private val p_order = new LogCPT( vocabulary + RootPair, dmv.attachmentOrder )
//   private val p_order = new LogCPT( Set[ObservedLabelPair](), dmv.attachmentOrder )
// 
//   //private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
//   //private val p_stop = new LogCPT( p_stop_keys, dmv.stopDecision )
//   private val p_stop = new LogCPT( Set[StopOrNot](), dmv.stopDecision )
// 
//   //private val p_choose_keys = dmv.chooseKeys( vocabulary )
//   private val p_choose = new LogCPT( Set[TwoStreamHeadChooseArg](), Set[ObservedLabel]() )
// 
//   def setParams( otherGram:DMVGrammar ) {
//     p_order.setCPT( otherGram.p_order.getCPT )
//     p_stop.setCPT( otherGram.p_stop.getCPT )
//     p_choose.setCPT( otherGram.p_choose.getCPT )
//   }
// 
//   def orderScore( word:ObservedLabelPair, pref:AttachmentOrder ) = p_order( word, pref )
//   def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) = p_stop( stopKey, stopDecision )
//   def chooseScore( chooseKey:TwoStreamHeadChooseArg, arg:ObservedLabel ) = p_choose( chooseKey, arg )
// 
//   def normalize {
//     p_order.normalize
//     p_stop.normalize
//     p_choose.normalize
//   }
// 
//   def setParams(
//     otherP_order:LogCPT[ObservedLabelPair,AttachmentOrder],
//     otherP_stop:LogCPT[StopOrNot,StopDecision],
//     otherP_choose:LogCPT[TwoStreamHeadChooseArg,ObservedLabel]
//   ) {
//     p_order.setCPT( otherP_order.getCPT )
//     p_stop.setCPT( otherP_stop.getCPT )
//     p_choose.setCPT( otherP_choose.getCPT )
//   }
// 
//   override def toString =
//     "P_Order:\n" + p_order +
//     "P_Stop:\n" + p_stop +
//     "P_Choose:\n" + p_choose
// 
// }
// 
