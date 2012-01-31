package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVBayesianBackoffGrammar
import predictabilityParsing.util.Math

/*
 *  This is a class for a DMV which backs off to acoustics for both head and argument. It is
 *  inspired by Will Headden's lexicalized EVG, in which the probabilities for each rule are drawn
 *  from a mixture of dirichlet priors, one which learns from the lexicalized distribution (which is
 *  sparse but informative) and one which learns from the unlexicalized distribution (which is less
 *  sparse but also less informative).
 *
 *  However, the details of Headden et al are probably inappropriate (since we're using acoustics
 *  instead of POS tags). Instead, we will draw our rules from a mixture of three distributions: one
 *  which conditions on both head and argument words, one which conditions on only head word, and
 *  one which conditions on no words. We should also look into backing off onto conditioning on only
 *  the argument word.
 *
 *  We will always use lexical identity for selecting sentence root.
 */
class DMVBayesianBackoffPartialCounts extends DMVPartialCounts {
  override def associatedGrammar = new DMVBayesianBackoffGrammar


  override def toDMVGrammar = {
    // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    val toReturn = associatedGrammar

    print( "Computing DMVBayesianBackoffGrammar..." )

    //println( "chooseCounts before toDMVGrammar:\n" + chooseCounts )

    val stopBackoff = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = StopOrNot( Word(h1), stopKey.dir, stopKey.adj )
            stopBackoff.setValue(
              backoffHeadKey,
              stopDecision,
              Math.sumLogProb(
                stopBackoff( backoffHeadKey, stopDecision ),
                stopCounts( stopKey, stopDecision )
              )
            )
          }
          case rootHead:AbstractRoot => {
          }
        }
      }
    }

    val chooseBackoffArg = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseBackoffBoth = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      //println( "Considering " + chooseKey )
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h1), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {
                chooseBackoffArg.setValue(
                  chooseKey,
                  Word(a1),
                  Math.sumLogProb(
                    chooseBackoffArg( chooseKey, Word(a1) ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                chooseBackoffBoth.setValue(
                  backoffHeadKey,
                  Word(a1),
                  Math.sumLogProb(
                    chooseBackoffBoth( backoffHeadKey, Word(a1) ),
                    chooseCounts( chooseKey, arg )
                  )
                )
              }
              case rootArg:AbstractRoot => {
              }
            }
          }
          case rootHead:AbstractRoot => {
          }
        }
      }
    }


    // println( "\n\nchooseCountsA:\n" + chooseCountsA )
    // println( "\n\nchooseCountsB:\n" + chooseCountsB + "\n\n" )

    stopCounts.expDigammaNormalize()
    stopBackoff.expDigammaNormalize()

    chooseCounts.expDigammaNormalize()
    chooseBackoffArg.expDigammaNormalize()
    chooseBackoffBoth.expDigammaNormalize()


    // Ok, now compute smoothed parameters
    val smoothedStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = StopOrNot( Word(h1), stopKey.dir, stopKey.adj )
            smoothedStop.setValue(
              stopKey,
              stopDecision,
              Math.sumLogProb(
                stopCounts( stopKey, stopDecision ),
                stopBackoff( backoffHeadKey, stopDecision )
              )
            )
          }
          case rootHead:AbstractRoot => {
            smoothedStop.setValue(
              stopKey,
              stopDecision,
              stopCounts( stopKey, stopDecision )
            )
          }
        }
      }
    }

    val smoothedChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h1), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {
                smoothedChoose.setValue(
                  chooseKey,
                  arg,
                  List(
                    chooseCounts( chooseKey, arg ),
                    chooseBackoffArg( chooseKey, Word(a1) ),
                    chooseBackoffBoth( backoffHeadKey, Word(a1) )
                  ).reduceLeft(Math.sumLogProb(_,_))
                )
              }
              case rootArg:AbstractRoot => {
                smoothedChoose.setValue(
                  chooseKey,
                  arg,
                  chooseCounts( chooseKey, arg )
                )
              }
            }
          }
          case rootHead:AbstractRoot => {
            smoothedChoose.setValue(
              chooseKey,
              arg,
              chooseCounts( chooseKey, arg )
            )
          }
        }
      }
    }


    toReturn.setParams(
      VanillaDMVParameters(
        orderCounts.toLogCPT,
        stopCounts.asLogCPT,
        chooseCounts.asLogCPT
        // stopCounts.toLogCPT,
        // chooseCounts.toLogCPT,
      )
    )

    println( "Done!" )

    //toReturn.normalize
    toReturn
  }

}

