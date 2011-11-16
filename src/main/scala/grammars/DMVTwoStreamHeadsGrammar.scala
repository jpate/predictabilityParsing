package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.partialCounts.DMVTwoStreamHeadsPartialCounts

class DMVTwoStreamHeadsGrammar extends DMVGrammar {

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    arg match {
      case WordPair( w1, _ ) => {
        p_choose( chooseKey, Word(w1) )
      }
      case root:RootState => {
        p_choose( chooseKey, arg )
      }
    }

  override def emptyPartialCounts = new DMVTwoStreamHeadsPartialCounts

}
