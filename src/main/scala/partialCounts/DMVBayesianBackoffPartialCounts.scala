package predictabilityParsing.partialCounts

import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVBayesianBackoffGrammar
import predictabilityParsing.grammars.AbstractDMVGrammar
import predictabilityParsing.util.Math

/*
 *  This is a class for a DMV which backs off to stream B for heads, emitting stream B only for
 *  arguments. It is inspired by Will Headden's lexicalized EVG, in which the probabilities for each
 *  rule are drawn from a mixture of dirichlet priors, one which learns from the lexicalized
 *  distribution (which is sparse but informative) and one which learns from the unlexicalized
 *  distribution (which is less sparse but also less informative).
 *
 *
 *  We will always use lexical identity for selecting sentence root.
 *
 */
class DMVBayesianBackoffPartialCounts(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  stopAlpha:Double = 1,
  chooseAlpha:Double = 1
) extends DMVPartialCounts {




  override def associatedGrammar = new DMVBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    //dmvRulesAlpha
    stopAlpha,
    chooseAlpha
  )


  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    // println( "stopCounts:\n" + stopCounts + "\n\n" )
    // println( "chooseCounts:\n" + chooseCounts + "\n\n" )

    val backoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )


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


    stopBackoffInterpolationSums.expDigammaNormalize( backoffAlphaMap )




    // whew, now let's get interpolation parameters for chooseScore

    val chooseBackoffHeadInterpolationSums =
      new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

    // along with backoff terms
    val noBackoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    val rootChooseCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( _, h2 ) => {
          val backoffHeadKey = ChooseArgument( Word( h2 ), chooseKey.dir )

          chooseCounts(chooseKey).keySet.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {

                val attachDir = chooseKey.dir


                chooseBackoffHeadInterpolationSums.setValue(
                  chooseKey,
                  NotBackoff,
                  logSum(
                    chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                // Also, count up for each backoff distribution (not the interpolation parameters).
                val backoffArg = Word(a2)

                noBackoffHeadCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    noBackoffHeadCounts( chooseKey, backoffArg ),
                    chooseCounts( chooseKey, arg )
                  )
                )
                backoffHeadCounts.setValue(
                  backoffHeadKey,
                  backoffArg,
                  logSum(
                    backoffHeadCounts( backoffHeadKey, backoffArg ),
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

              case WordPair( _, d2 ) => {
                rootChooseCounts.setValue(
                  chooseKey,
                  Word( d2 ),
                  logSum(
                    rootChooseCounts( chooseKey, Word( d2 ) ),
                    chooseCounts( chooseKey, arg )
                  )
                )
              }

            }
          }
        }
      }
    }

    chooseBackoffHeadInterpolationSums.parents.foreach{ chooseKey =>
      chooseBackoffHeadInterpolationSums.setValue(
        chooseKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    chooseBackoffHeadInterpolationSums.expDigammaNormalize( backoffAlphaMap )

    stopNoBackoffCounts.expDigammaNormalize( stopAlpha, alphaUnk = false )
    stopBackoffCounts.expDigammaNormalize( stopAlpha, alphaUnk = false )

    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val defaultStopParentMap = collection.mutable.Map[StopOrNot,Double]()

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
            defaultStopParentMap += ( stopKey -> logSum(
              stopBackoffInterpolationSums( stopKey, NotBackoff ) +
                stopNoBackoffCounts.getParentDefault( stopKey ),
              stopBackoffInterpolationSums( stopKey, Backoff ) +
                stopBackoffCounts.getParentDefault( backoffHeadKey )
            ) )


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

    backedoffStop.setDefaultParentMap( defaultStopParentMap )

    backoffHeadCounts.expDigammaNormalize( chooseAlpha )
    noBackoffHeadCounts.expDigammaNormalize( chooseAlpha )
    rootChooseCounts.expDigammaNormalize( chooseAlpha )


    val argVocab = chooseCounts.values.flatMap{ _.keySet }.toSet.map{ pair:ObservedLabel =>
      pair match {
        case WordPair( _, d2 ) => Word( d2 )
      }
    }


    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val defaultChooseParentMap = collection.mutable.Map[ChooseArgument,Double]()

    chooseCounts.parents.foreach{ chooseKey =>

      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
            arg match {
              case WordPair( _, a2 ) => {

                val backoffArg = Word(a2)

                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                      chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ) +
                        noBackoffHeadCounts( chooseKey, backoffArg ),
                      chooseBackoffHeadInterpolationSums( chooseKey, Backoff ) +
                        backoffHeadCounts( backoffHeadKey, backoffArg )
                  )
                )
                defaultChooseParentMap += ( chooseKey -> logSum(
                  chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ) +
                    noBackoffHeadCounts.getParentDefault( chooseKey ),
                  chooseBackoffHeadInterpolationSums( chooseKey, Backoff ) +
                    backoffHeadCounts.getParentDefault( backoffHeadKey )
                ) )
              }
              case rootArg:AbstractRoot => { /* Intentionally empty */ }
            }
          }
          case rootHead:AbstractRoot => {
            arg match {
              case WordPair( _, d2 ) =>
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  rootChooseCounts( chooseKey, Word( d2 ) )
                )
              case rootArg:AbstractRoot =>
            }
          }
        }
      }
    }

    backedoffChoose.setDefaultParentMap( defaultChooseParentMap )


    println( "Done!" )

    val toReturn = associatedGrammar


    toReturn.setParams(
      DMVBayesianBackoffParameters(
        backedoffStop.asLogCPT,
        //new LogCPT( Set[StopOrNot](), dmv.stopDecision ),
        backedoffChoose.asLogCPT,
        //new LogCPT( Set[ChooseArgument](), Set[ObservedLabel]() ),
        stopBackoffInterpolationSums,
        stopNoBackoffCounts,
        stopBackoffCounts,
        chooseBackoffHeadInterpolationSums,
        noBackoffHeadCounts,
        backoffHeadCounts,
        rootChooseCounts
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

