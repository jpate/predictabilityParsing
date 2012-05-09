package predictabilityParsing.partialCounts

import scalala.library.Numerics.{lgamma,logSum}
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVBayesianBackoffThreeStreamGrammar
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
class DMVBayesianBackoffThreeStreamPartialCounts(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffOneAlpha:Double = 52.5,
  backoffTwoAlpha:Double = 70
) extends DMVPartialCounts {


  override def associatedGrammar = new DMVBayesianBackoffThreeStreamGrammar(
    noBackoffAlpha,
    backoffOneAlpha,
    backoffTwoAlpha
  )

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    //val backoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val firstBackoffAlphaMap = Map( Backoff -> backoffOneAlpha, NotBackoff -> noBackoffAlpha )
    val secondBackoffAlphaMap = Map( Backoff -> backoffTwoAlpha, NotBackoff -> noBackoffAlpha )


    // First, compute new interpolation parameters, starting with stop
    val stopBackoffFirstInterpolationSums = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )
    val stopBackoffSecondInterpolationSums = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffOnceCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffTwiceCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordTriple( h1, h2, h3 ) => {
          dmv.stopDecision.foreach{ dec =>

            val backoffHeadOnce = WordPair(h2, h3)
            val backoffStopOnceKey = StopOrNot( backoffHeadOnce, stopKey.dir, stopKey.adj )

            val backoffHeadTwice = Word(h3)
            val backoffStopTwiceKey = StopOrNot( backoffHeadTwice, stopKey.dir, stopKey.adj )

            // interpolaton parameters. We'll stop backing off to the extent that the observed trees
            // are estimated to have high probability (i.e. we're adding the estimated spans to only
            // the NotBackoff outcome of backoff dirichlets.)
            stopBackoffFirstInterpolationSums.setValue(
              stopKey,
              NotBackoff,
              logSum(
                stopBackoffFirstInterpolationSums( stopKey, NotBackoff ),
                stopCounts( stopKey, dec )
              )
            )
            stopBackoffSecondInterpolationSums.setValue(
              backoffStopOnceKey,
              NotBackoff,
              logSum(
                stopBackoffSecondInterpolationSums( stopKey, NotBackoff ),
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
            stopBackoffOnceCounts.setValue(
              backoffStopOnceKey,
              dec,
              logSum(
                stopBackoffOnceCounts( backoffStopOnceKey, dec ),
                stopCounts( stopKey, dec )
              )
            )
            stopBackoffTwiceCounts.setValue(
              backoffStopOnceKey,
              dec,
              logSum(
                stopBackoffTwiceCounts( backoffStopTwiceKey, dec ),
                stopCounts( stopKey, dec )
              )
            )

          }
        }
        case rootHead:AbstractRoot => { /* intentionally empty */ }
      }
    }

    stopBackoffFirstInterpolationSums.parents.foreach{ stopKey =>
      stopBackoffFirstInterpolationSums.setValue(
        stopKey,
        Backoff,
        Double.NegativeInfinity
      )
    }
    stopBackoffSecondInterpolationSums.parents.foreach{ stopKey =>
      stopBackoffSecondInterpolationSums.setValue(
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


    stopBackoffFirstInterpolationSums.expDigammaNormalize( firstBackoffAlphaMap )
    stopBackoffSecondInterpolationSums.expDigammaNormalize( secondBackoffAlphaMap )

    stopBackoffFirstInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffOneAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffOneAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffOneAlpha) )
        }
      )
    )
    stopBackoffSecondInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffTwoAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffTwoAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffTwoAlpha) )
        }
      )
    )



    // whew, now let's get interpolation parameters for chooseScore

    val chooseBackoffHeadFirstInterpolationSums =
      new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )
    val chooseBackoffHeadSecondInterpolationSums =
      new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

    // along with backoff terms
    val noBackoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffOnceHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffTwiceHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    val rootChooseCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordTriple( h1, h2, h3 ) => {
          val backoffHeadOnce = WordPair(h2, h3)
          val backoffHeadOnceKey = ChooseArgument( backoffHeadOnce, chooseKey.dir )

          val backoffHeadTwice = Word(h3)
          val backoffHeadTwiceKey = ChooseArgument( backoffHeadTwice, chooseKey.dir )


          chooseCounts(chooseKey).keySet.foreach{ arg =>
            arg match {
              case WordTriple( a1, a2, a3 ) => {

                val attachDir = chooseKey.dir


                chooseBackoffHeadFirstInterpolationSums.setValue(
                  chooseKey,
                  NotBackoff,
                  logSum(
                    chooseBackoffHeadFirstInterpolationSums( chooseKey, NotBackoff ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                chooseBackoffHeadSecondInterpolationSums.setValue(
                  backoffHeadOnceKey,
                  NotBackoff,
                  logSum(
                    chooseBackoffHeadSecondInterpolationSums( backoffHeadOnceKey, NotBackoff ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                // Also, count up for each backoff distribution (not the interpolation parameters).
                val backoffArg = Word(a3)

                noBackoffHeadCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    noBackoffHeadCounts( chooseKey, backoffArg ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                backoffOnceHeadCounts.setValue(
                  backoffHeadOnceKey,
                  backoffArg,
                  logSum(
                    backoffOnceHeadCounts( backoffHeadOnceKey, backoffArg ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                backoffTwiceHeadCounts.setValue(
                  backoffHeadTwiceKey,
                  backoffArg,
                  logSum(
                    backoffTwiceHeadCounts( backoffHeadTwiceKey, backoffArg ),
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
            arg match {

              case WordTriple( _, _, d3 ) => {
                val argC = Word( d3 )
                rootChooseCounts.setValue(
                  chooseKey,
                  argC,
                  logSum(
                    rootChooseCounts( chooseKey, argC ),
                    chooseCounts( chooseKey, arg )
                  )
                )
              }

            }
          }
        }
      }
    }


    chooseBackoffHeadFirstInterpolationSums.parents.foreach{ chooseKey =>
      chooseBackoffHeadFirstInterpolationSums.setValue(
        chooseKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    chooseBackoffHeadSecondInterpolationSums.parents.foreach{ chooseKey =>
      chooseBackoffHeadSecondInterpolationSums.setValue(
        chooseKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    chooseBackoffHeadFirstInterpolationSums.expDigammaNormalize( firstBackoffAlphaMap )
    chooseBackoffHeadFirstInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffOneAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffOneAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffOneAlpha) )
        }
      )
    )

    chooseBackoffHeadSecondInterpolationSums.expDigammaNormalize( secondBackoffAlphaMap )
    chooseBackoffHeadSecondInterpolationSums.setDefaultChildMap(
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffTwoAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffTwoAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffTwoAlpha) )
        }
      )
    )


    // Ok, now compute backed-off parameters

    stopNoBackoffCounts.expDigammaNormalize()
    stopBackoffOnceCounts.expDigammaNormalize()
    stopBackoffTwiceCounts.expDigammaNormalize()

    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordTriple( _, h2, h3 ) => {
            val backoffHeadOnce = WordPair(h2, h3)
            val backoffStopOnceKey = StopOrNot( backoffHeadOnce, stopKey.dir, stopKey.adj )

            val backoffHeadTwice = Word(h3)
            val backoffStopTwiceKey = StopOrNot( backoffHeadTwice, stopKey.dir, stopKey.adj )


            backedoffStop.setValue(
              stopKey,
              stopDecision,

              logSum(
                stopBackoffFirstInterpolationSums( stopKey, NotBackoff ) +
                  stopNoBackoffCounts( stopKey, stopDecision ),

                stopBackoffFirstInterpolationSums( stopKey, Backoff ) +
                  stopBackoffSecondInterpolationSums( backoffStopOnceKey, NotBackoff ) +
                    stopBackoffOnceCounts( backoffStopOnceKey, stopDecision ),

                stopBackoffFirstInterpolationSums( stopKey, Backoff ) +
                  stopBackoffSecondInterpolationSums( backoffStopOnceKey, Backoff ) +
                    stopBackoffTwiceCounts( backoffStopTwiceKey, stopDecision )

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

    backedoffStop.setDefaultChildMap(
      Map[StopDecision,Double](
        NotStop -> {
          Math.expDigamma( 0 ) - Math.expDigamma( math.log( 2 ) )
        },
        Stop -> {
          Math.expDigamma( 0 ) - Math.expDigamma( math.log( 2 ) )
        }
      )
    )


    noBackoffHeadCounts.expDigammaNormalize()
    backoffOnceHeadCounts.expDigammaNormalize()
    backoffTwiceHeadCounts.expDigammaNormalize()
    rootChooseCounts.expDigammaNormalize()



    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>

      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordTriple( _, h2, h3 ) => {

            val backoffHeadOnce = WordPair(h2, h3)
            val backoffHeadOnceKey = ChooseArgument( backoffHeadOnce, chooseKey.dir )

            val backoffHeadTwice = Word(h3)
            val backoffHeadTwiceKey = ChooseArgument( backoffHeadTwice, chooseKey.dir )

            arg match {
              case WordTriple( _, _, a3 ) => {

                val backoffArg = Word(a3)

                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(
                      chooseBackoffHeadFirstInterpolationSums( chooseKey, NotBackoff ) +
                        noBackoffHeadCounts( chooseKey, backoffArg ),

                      chooseBackoffHeadFirstInterpolationSums( chooseKey, Backoff ) +
                        chooseBackoffHeadSecondInterpolationSums( backoffHeadOnceKey, NotBackoff ) +
                          backoffOnceHeadCounts( backoffHeadOnceKey, backoffArg ),

                      chooseBackoffHeadFirstInterpolationSums( chooseKey, Backoff ) +
                        chooseBackoffHeadSecondInterpolationSums( backoffHeadOnceKey, Backoff ) +
                          backoffTwiceHeadCounts( backoffHeadTwiceKey, backoffArg )
                    )
                  )
                )
              }
              case rootArg:AbstractRoot => { /* Intentionally empty */ }
            }
          }
          case rootHead:AbstractRoot => {
            arg match {
              case WordTriple( _, _, d3 ) => {
                val argC = Word( d3 )
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  rootChooseCounts( chooseKey, argC )
                )
              }
              case rootArg:AbstractRoot =>
            }

          }
        }
      }
    }


    println( "Done!" )

    val toReturn = associatedGrammar


    toReturn.setParams(
      DMVBayesianBackoffThreeStreamParameters(
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        stopBackoffFirstInterpolationSums,
        stopBackoffSecondInterpolationSums,
        stopNoBackoffCounts,
        stopBackoffOnceCounts,
        stopBackoffTwiceCounts,
        chooseBackoffHeadFirstInterpolationSums,
        chooseBackoffHeadSecondInterpolationSums,
        noBackoffHeadCounts,
        backoffOnceHeadCounts,
        backoffTwiceHeadCounts,
        rootChooseCounts
      )
    )

    toReturn

  }

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffOneAlpha: " + backoffOneAlpha + "\n" +
      "\tbackoffTwoAlpha: " + backoffTwoAlpha + "\n"


}

