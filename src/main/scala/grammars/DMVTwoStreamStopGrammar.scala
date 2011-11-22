package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVTwoStreamStopPartialCounts

class DMVTwoStreamStopGrammar extends DMVGrammar {


  override def emptyPartialCounts = new DMVTwoStreamStopPartialCounts

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    chooseKey.h match {
      case WordPair( h1, _ ) => {
        val singleStreamChoooseKey = ChooseArgument( Word(h1), chooseKey.dir )
        arg match {
          case WordPair( a1, _ ) => {
            p_choose( singleStreamChoooseKey, Word(a1) ) 
          }
          case rootArg:AbstractRoot => {
            p_choose( singleStreamChoooseKey, arg )
          }
        }
      }
      case rootHead:AbstractRoot => {
        arg match {
          case WordPair( a1, _ ) => {
            p_choose( chooseKey, Word(a1) ) 
          }
          case rootArg:AbstractRoot => {
            p_choose( chooseKey, arg )
          }
        }
      }
    }

}
