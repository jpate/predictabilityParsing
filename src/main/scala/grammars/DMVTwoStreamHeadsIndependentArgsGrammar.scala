package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVTwoStreamHeadsIndependentArgsPartialCounts

class DMVTwoStreamHeadsIndependentArgsGrammar extends AbstractDMVGrammar {
  private val p_chooseB = new LogCPT( Set[ChooseArgument](), Set[ObservedLabel]() )

  def getParams = DMVIndependentStreamHeadsParameters( p_order, p_stop, p_choose, p_chooseB)

  override def emptyPartialCounts = new DMVTwoStreamHeadsIndependentArgsPartialCounts

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    arg match {
      case WordPair( a1, a2 ) => {
        p_choose( chooseKey, Word(a1) ) + p_chooseB( chooseKey, Word( a2 ) )
      }
      case rootArg:AbstractRoot => {
        p_choose( chooseKey, arg ) //+ p_chooseB( chooseKey, arg )
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
    p_order.setCPT( newP_order /*.getCPT*/ )
    p_stop.setCPT( newP_stop /*.getCPT*/ )
    p_choose.setCPT( newP_choose /*.getCPT*/ )
    p_chooseB.setCPT( newP_chooseB /*.getCPT*/ )
  }
}
