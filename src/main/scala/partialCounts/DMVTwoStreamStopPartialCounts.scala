package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVTwoStreamStopGrammar
import predictabilityParsing.util.Math

class DMVTwoStreamStopPartialCounts extends DMVPartialCounts {

  override def associatedGrammar = new DMVTwoStreamStopGrammar

  override def incrementChooseCounts(chooseKey:ChooseArgument,arg:ObservedLabel,increment:Double) {
    chooseKey.h match {
      case WordPair( h1, _ ) => {
        val singleStreamChoooseKey = ChooseArgument( Word(h1), chooseKey.dir )
        arg match {
          case WordPair( a1, _ ) =>
            chooseCounts.setValue(
              singleStreamChoooseKey,
              Word( a1 ),
              Math.sumLogProb( chooseCounts( singleStreamChoooseKey, Word(a1) ), increment )
            )
          case otherArg:ObservedLabel =>
            chooseCounts.setValue(
              singleStreamChoooseKey,
              arg,
              Math.sumLogProb( chooseCounts( singleStreamChoooseKey, arg ), increment )
            )
        }
      }
      case otherHead:ObservedLabel =>
        arg match {
          case WordPair( a1, _ ) =>
            chooseCounts.setValue(
              chooseKey,
              Word( a1 ),
              Math.sumLogProb( chooseCounts( chooseKey, Word(a1) ), increment )
            )
          case otherArg:ObservedLabel =>
            chooseCounts.setValue(
              chooseKey,
              arg,
              Math.sumLogProb( chooseCounts( chooseKey, arg ), increment )
            )
        }
    }
  }

  /*
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
  */

}

