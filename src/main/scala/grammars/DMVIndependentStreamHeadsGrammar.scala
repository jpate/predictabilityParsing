package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVIndependentStreamHeadsPartialCounts

class DMVIndependentStreamHeadsGrammar extends AbstractDMVGrammar {
  private val p_chooseB = new LogCPT( Set[ChooseArgument](), Set[ObservedLabel]() )

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    chooseKey.h match {
      case WordPair( h1, h2 ) => {
        val chooseA = ChooseArgument( Word(h1), chooseKey.dir )
        val chooseB = ChooseArgument( Word(h2), chooseKey.dir )

        arg match {
          case WordPair( w1, _ ) => {
            p_choose( chooseA, Word(w1) ) + p_chooseB( chooseB, Word( w1 ) )
          }
          case rootArg:AbstractRoot => {
            p_choose( chooseA, arg ) + p_chooseB( chooseB, arg )
          }
        }
      }
      case rootHead:AbstractRoot => {
        arg match {
          case WordPair( w1, _ ) => {
            p_choose( chooseKey, Word(w1) )
          }
          case rootArg:AbstractRoot => {
            p_choose( chooseKey, arg )
          }
        }
      }
    }
  
  override def normalize {
    p_order.normalize
    p_stop.normalize
    p_choose.normalize
    p_chooseB.normalize
  }

  def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVIndependentStreamHeadsParameters(
      newP_order,
      newP_stop,
      newP_choose,
      newP_chooseB
    ) = parameters
    p_order.setCPT( newP_order.getCPT )
    p_stop.setCPT( newP_stop.getCPT )
    p_choose.setCPT( newP_choose.getCPT )
    p_chooseB.setCPT( newP_chooseB.getCPT )
  }

      // def setParams( otherGram:DMVIndependentStreamHeadsGrammar ) {
      //   p_order.setCPT( otherGram.p_order.getCPT )
      //   p_stop.setCPT( otherGram.p_stop.getCPT )
      //   p_choose.setCPT( otherGram.p_choose.getCPT )
      //   p_chooseB.setCPT( otherGram.p_chooseB.getCPT )
      // }

  def setParams( otherGram:DMVIndependentStreamHeadsGrammar ) {
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
    p_chooseB.setCPT( otherGram.p_chooseB.getCPT )
  }

  def setParams(
    otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
    otherP_stop:LogCPT[StopOrNot,StopDecision],
    otherP_choose:LogCPT[ChooseArgument,ObservedLabel],
    otherP_chooseB:LogCPT[ChooseArgument,ObservedLabel]
  ) {
    p_order.setCPT( otherP_order.getCPT )
    p_stop.setCPT( otherP_stop.getCPT )
    p_choose.setCPT( otherP_choose.getCPT )
    p_chooseB.setCPT( otherP_chooseB.getCPT )
  }

  def getParams = DMVIndependentStreamHeadsParameters( p_order, p_stop, p_choose, p_chooseB)

  override def emptyPartialCounts = new DMVIndependentStreamHeadsPartialCounts

  override def toString =
    "P_Order:\n" + p_order +
    "P_Stop:\n" + p_stop +
    "P_ChooseA:\n" + p_choose +
    "P_ChooseB:\n" + p_chooseB
}
