package predictabilityParsing.partialCounts

import scalala.library.Numerics.{lgamma,logSum}
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVBothBayesianBackoffGrammar
import predictabilityParsing.grammars.AbstractDMVGrammar
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
 *
 */
class DMVBothBayesianBackoffPartialCounts(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70
) extends DMVPartialCounts {


  override def associatedGrammar = new DMVBothBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha
  )

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    val stopBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val headBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val argBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )


    // First, compute new interpolation parameters, starting with stop
    val stopBackoffInterpolationSums = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1, h2 ) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffHead = Word(h2)
            val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )


            // interpolaton parameters. We'll stop backing off to the extent that the observed trees
            // are estimated to have high probability (i.e. we're adding the estimated spans to only
            // the NotBackoff outcome of backoff dirichlets.)
            stopBackoffInterpolationSums.setValue(
              stopKey,
              NotBackoff,
              logSum(
                stopBackoffInterpolationSums( stopKey, NotBackoff ),
                stopCounts( stopKey, dec )
              )
            )

            // for backoff distribution, only sum over elements of the backoff set.
            stopNoBackoffCounts.setValue(
              stopKey,
              dec,
              logSum(
                stopNoBackoffCounts( stopKey, dec ),
                stopCounts( stopKey, dec )
              )
            )
            stopBackoffCounts.setValue(
              backoffStopKey,
              dec,
              logSum(
                stopBackoffCounts( backoffStopKey, dec ),
                stopCounts( stopKey, dec )
              )
            )

          }
        }
        case rootHead:AbstractRoot => { /* intentionally empty */ }
      }
    }

    stopBackoffInterpolationSums.parents.foreach{ stopKey =>
      val WordPair( h1, h2 ) = stopKey.w
      val backoffHead = Word(h2)
      val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

      stopBackoffInterpolationSums.setValue(
        stopKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    val rootStopCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      Stop,
      Double.NegativeInfinity
    )
    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      NotStop,
      0D
    )

    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      Stop,
      0D
    )
    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )


    rootStopCounts.setValue(
      StopOrNot( Root, RightAttachment, true ),
      Stop,
      0D
    )
    rootStopCounts.setValue(
      StopOrNot( Root, RightAttachment, true ),
      NotStop,
      Double.NegativeInfinity
    )
    rootStopCounts.setValue(
      StopOrNot( Root, RightAttachment, false ),
      Stop,
      0D
    )
    rootStopCounts.setValue(
      StopOrNot( Root, RightAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )


    stopBackoffInterpolationSums.expDigammaNormalize( stopBackoffAlphaMap )
    stopBackoffInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    )



    // whew, now let's get interpolation parameters for chooseScore

    val headBackoffInterpolationSums = new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )
    val argBackoffInterpolationSums = new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

    // along with backoff terms, both jointly estimated and with independence assumptions.
    val noChooseBackoffCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffArgBCounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )
    val backoffBothArgBCounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )

    val rootChooseCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( h1, h2 ) => {

          val attachDir = chooseKey.dir
          val backoffHead = Word( h2 )
          val backoffHeadKey = ChooseArgument( backoffHead, attachDir )

          chooseCounts(chooseKey).keySet.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {


                val argPair = WordPair( a1, a2 )
                val argA = Word(a1)
                val argB = Word(a2)

                val backoffArg = WordQuad( h1, h2, a1, a2 )
                val backoffArgKey = ChooseArgument( backoffArg, attachDir )


                headBackoffInterpolationSums.setValue(
                  chooseKey,
                  NotBackoff,
                  logSum(
                    headBackoffInterpolationSums( chooseKey, NotBackoff ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                argBackoffInterpolationSums.setValue(
                  backoffArgKey,
                  NotBackoff,
                  logSum(
                    argBackoffInterpolationSums( backoffArgKey, NotBackoff ),
                    chooseCounts( chooseKey, arg )
                  )
                )



                // Also, count up for each backoff distribution (not the interpolation parameters).
                noChooseBackoffCounts.setValue(
                  chooseKey,
                  argPair,
                  logSum(
                    noChooseBackoffCounts( chooseKey, argPair ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                backoffHeadCounts.setValue(
                  backoffHeadKey,
                  argPair,
                  logSum(
                    backoffHeadCounts( backoffHeadKey, argPair ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                backoffArgBCounts.setValue(
                  chooseKey,
                  argB,
                  logSum(
                    backoffArgBCounts( chooseKey, argB ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                backoffBothArgBCounts.setValue(
                  backoffHeadKey,
                  argB,
                  logSum(
                    backoffBothArgBCounts( backoffHeadKey, argB ),
                    chooseCounts( chooseKey, arg )
                  )
                )

              }
              case rootArg:AbstractRoot => {
                assert( chooseCounts( chooseKey, arg ) == Double.NegativeInfinity )
              }
            }
          }
        }
        case rootHead:AbstractRoot => {
          chooseCounts(chooseKey).keySet.foreach{ arg =>
            rootChooseCounts.setValue(
              chooseKey,
              arg,
              logSum(
                rootChooseCounts( chooseKey, arg ),
                chooseCounts( chooseKey, arg )
              )
            )
          }
        }
      }
    }



    headBackoffInterpolationSums.parents.foreach{ headBackoffKey =>
      headBackoffInterpolationSums.setValue(
        headBackoffKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    argBackoffInterpolationSums.parents.foreach{ argBackoffKey =>
      argBackoffInterpolationSums.setValue(
        argBackoffKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    headBackoffInterpolationSums.expDigammaNormalize( headBackoffAlphaMap )
    headBackoffInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    )

    argBackoffInterpolationSums.expDigammaNormalize( argBackoffAlphaMap )
    argBackoffInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    )




    // Ok, now compute backed-off parameters

    stopNoBackoffCounts.expDigammaNormalize()
    stopBackoffCounts.expDigammaNormalize()

    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = StopOrNot( Word(h2), stopKey.dir, stopKey.adj )
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              logSum(
                stopBackoffInterpolationSums( stopKey, NotBackoff ) + stopNoBackoffCounts( stopKey, stopDecision ),
                stopBackoffInterpolationSums( stopKey, Backoff ) + stopBackoffCounts( backoffHeadKey, stopDecision )
              )
            )

          }
          case rootHead:AbstractRoot => {
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              rootStopCounts( stopKey, stopDecision )
            )
          }
        }
      }
    }

    backedoffStop.setDefault(
      expDigamma( 0D ) - expDigamma( math.log( backedoffStop.parents.size ) )
    )


    noChooseBackoffCounts.expDigammaNormalize()
    backoffHeadCounts.expDigammaNormalize()
    backoffArgBCounts.expDigammaNormalize()
    backoffBothArgBCounts.expDigammaNormalize()
    rootChooseCounts.expDigammaNormalize()


    val chooseDefaults = collection.mutable.Map[ChooseArgument,Double]().withDefaultValue( Double.NegativeInfinity )

    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      val attachDir = chooseKey.dir

      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
          val attachDir = chooseKey.dir
          val backoffHead = Word( h2 )
          val backoffHeadKey = ChooseArgument( backoffHead, attachDir )

            arg match {
              case WordPair( a1, a2 ) => {

                val argPair = WordPair( a1, a2 )
                val argA = Word(a1)
                val argB = Word(a2)

                val argBackoff = WordQuad( h1, h2, a1, a2 )
                val argBackoffKey =
                  ChooseArgument( argBackoff, attachDir )

                chooseDefaults +=
                  chooseKey -> logSum(
                    Seq(

                      chooseDefaults( chooseKey ),

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          noChooseBackoffCounts.getParentDefault( chooseKey ),

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffArgBCounts.getParentDefault( chooseKey ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          backoffHeadCounts.getParentDefault( backoffHeadKey ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffBothArgBCounts.getParentDefault( chooseKey )

                    )
                  )





                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          noChooseBackoffCounts( chooseKey, argPair ),

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffArgBCounts( chooseKey, argB ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          backoffHeadCounts( backoffHeadKey, argPair ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffBothArgBCounts( chooseKey, argB )

                    )

                  )
                )
              }
              case rootArg:AbstractRoot => { /* intentionally empty */ }
            }
          }
          case rootHead:AbstractRoot => {
            backedoffChoose.setValue(
              chooseKey,
              arg,
              rootChooseCounts( chooseKey, arg )
            )
          }
        }
      }
    }

    backedoffChoose.setDefault(
      expDigamma( 0D ) - expDigamma( math.log( backedoffChoose.parents.size ) )
    )
    backedoffChoose.setDefaultParentMap( chooseDefaults )


    println( "Done!" )

    val toReturn = associatedGrammar


    toReturn.setParams(
      VanillaDMVParameters(
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT
      )
    )

    toReturn

  }

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffAlpha: " + backoffAlpha + "\n"


}
