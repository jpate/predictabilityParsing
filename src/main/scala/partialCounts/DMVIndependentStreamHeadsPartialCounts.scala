package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVIndependentStreamHeadsGrammar
import predictabilityParsing.util.Math

class DMVIndependentStreamHeadsPartialCounts extends DMVPartialCounts {

  override def associatedGrammar = new DMVIndependentStreamHeadsGrammar

  override def toDMVGrammar = {
    // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    val toReturn = associatedGrammar

    //println( "chooseCounts before toDMVGrammar:\n" + chooseCounts )

    val chooseCountsA = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseCountsB = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      //println( "Considering " + chooseKey )
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        //println( "    Considering " + arg )
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val chooseA = ChooseArgument( Word(h1), chooseKey.dir )
            val chooseB = ChooseArgument( Word(h2), chooseKey.dir )
            arg match {
              case WordPair( a1, _ ) => {
                chooseCountsA.setValue(
                  chooseA,
                  Word( a1 ),
                  Math.sumLogProb(
                    chooseCountsA( chooseA, Word( a1 ) ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                chooseCountsB.setValue(
                  chooseB,
                  Word( a1 ),
                  Math.sumLogProb(
                    chooseCountsB( chooseB, Word( a1 ) ),
                    chooseCounts( chooseKey, arg )
                  )
                )
              }

              case rootArg:AbstractRoot => {
                chooseCountsA.setValue(
                  chooseA,
                  arg,
                  Math.sumLogProb(
                    chooseCountsA( chooseA, arg ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                chooseCountsB.setValue(
                  chooseB,
                  arg,
                  Math.sumLogProb(
                    chooseCountsB( chooseB, arg ),
                    chooseCounts( chooseKey, arg )
                  )
                )
              }
            }
          }
          case rootHead:AbstractRoot => {
            arg match {
              case WordPair( a1, _ ) => {
                chooseCountsA.setValue(
                  chooseKey,
                  Word( a1 ),
                  Math.sumLogProb(
                    chooseCountsA( chooseKey, Word( a1 ) ),
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

