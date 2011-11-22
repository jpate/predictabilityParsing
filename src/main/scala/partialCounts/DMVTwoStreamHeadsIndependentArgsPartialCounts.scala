package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVTwoStreamHeadsIndependentArgsGrammar
import predictabilityParsing.util.Math

class DMVTwoStreamHeadsIndependentArgsPartialCounts extends DMVPartialCounts {

  override def associatedGrammar = new DMVTwoStreamHeadsIndependentArgsGrammar

  override def toDMVGrammar = {
    // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    val toReturn = associatedGrammar

    //println( "chooseCounts before toDMVGrammar:\n" + chooseCounts )

    val chooseCountsA = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseCountsB = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      //println( "Considering " + chooseKey )
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        arg match {
          case WordPair( a1, a2 ) => {
            chooseCountsA.setValue(
              chooseKey,
              Word( a1 ),
              Math.sumLogProb(
                chooseCountsA( chooseKey, Word( a1 ) ),
                chooseCounts( chooseKey, arg )
              )
            )
            chooseCountsB.setValue(
              chooseKey,
              Word( a2 ),
              Math.sumLogProb(
                chooseCountsB( chooseKey, Word( a2 ) ),
                chooseCounts( chooseKey, arg )
              )
            )
          }
          case rootArg:AbstractRoot => {
            chooseCountsA.setValue(
              chooseKey,
              arg,
              Math.sumLogProb(
                chooseCountsA( chooseKey, arg ),
                chooseCounts( chooseKey, arg )
              )
            )
          }
        }
      }
    }

    // println( "\n\nchooseCountsA:\n" + chooseCountsA )
    // println( "\n\nchooseCountsB:\n" + chooseCountsB + "\n\n" )

    toReturn.setParams(
      DMVIndependentStreamHeadsParameters(
        orderCounts.toLogCPT,
        stopCounts.toLogCPT,
        chooseCountsA.toLogCPT,
        chooseCountsB.toLogCPT
      )
    )

    toReturn.normalize
    toReturn
  }

}

