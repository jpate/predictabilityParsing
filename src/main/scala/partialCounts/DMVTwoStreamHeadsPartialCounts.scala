package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVTwoStreamHeadsGrammar
import predictabilityParsing.util.Math

class DMVTwoStreamHeadsPartialCounts extends DMVPartialCounts {
  override def associatedGrammar = new DMVTwoStreamHeadsGrammar
  override def incrementChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, increment:Double ) {
    arg match {
      case WordPair( w1, _ ) => {
        chooseCounts.setValue(
          chooseKey,
          Word(w1),
          Math.sumLogProb( chooseCounts(chooseKey,Word(w1)), increment )
        )
      }
      case other:ObservedLabel => {
        chooseCounts.setValue(
          chooseKey,
          arg,
          Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
        )
      }
    }
  }
}

