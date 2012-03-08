package predictabilityParsing.partialCounts

import scalala.library.Numerics.{lgamma,logSum}
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVBayesianBackoffGrammar
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
 *  Finally, I apologize for the huge number of parameters in the class constructor...
 *
 */
class DMVBayesianBackoffPartialCounts(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noStopBackoff:Double = 35,
  stopBackoff:Double = 70,
  noChooseBackoff:Double = 30,
  backoffHead:Double = 60,
  backoffArg:Double = 60,
  backoffBoth:Double = 120,
  // these are specific backoff parameters
  noStopBackoffScore:AbstractLog1dTable[StopOrNot],
  stopBackoffScore:AbstractLog1dTable[StopOrNot],
  noChooseBackoffScore:AbstractLog1dTable[ChooseArgument],
  backoffHeadScore:AbstractLog1dTable[ChooseArgument],
  backoffArgScore:AbstractLog1dTable[ChooseArgument],
  backoffBothScore:AbstractLog1dTable[ChooseArgument]
) extends DMVPartialCounts {


  def this(
    noStopBackoff:Double,
    stopBackoff:Double,
    noChooseBackoff:Double,
    backoffHead:Double,
    backoffArg:Double,
    backoffBoth:Double
  ) = this(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffHead,
    backoffArg,
    backoffBoth,
    // these are specific backoff parameters
    noStopBackoffScore = Log1dTable(
      Set[StopOrNot](),
      Math.expDigamma( math.log( noStopBackoff ) ) -
        Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    ),
    stopBackoffScore = Log1dTable(
      Set[StopOrNot](),
      Math.expDigamma( math.log( stopBackoff ) ) -
        Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    ),
    noChooseBackoffScore = Log1dTable(
      Set[ChooseArgument](),
      Math.expDigamma( math.log( noChooseBackoff ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffHeadScore = Log1dTable(
      Set[ChooseArgument](),
      Math.expDigamma( math.log( backoffHead ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffArgScore = Log1dTable(
      Set[ChooseArgument](),
      Math.expDigamma( math.log( backoffArg ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffBothScore = Log1dTable(
      Set[ChooseArgument](),
      Math.expDigamma( math.log( backoffBoth ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    )
  )
  def this() = this( 35, 70, 30, 60, 60, 120 ) // defaults inspired by Headden for use on wsj10

  override def clearInterpolationScores {
    println( "clearing interpolation scores..." )
    noStopBackoffScore.setPT(
      Log1dTable(
        Set[StopOrNot](),
        Math.expDigamma( math.log( noStopBackoff ) ) -
          Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      )//.getPT
    )
    stopBackoffScore.setPT(
      Log1dTable(
        Set[StopOrNot](),
        Math.expDigamma( math.log( stopBackoff ) ) -
          Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      )//.getPT
    )
    noChooseBackoffScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( noChooseBackoff ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    backoffHeadScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffArg ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    backoffArgScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffArg ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      ) //.getPT
    )
    backoffBothScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffBoth ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      ) //.getPT
    )
  }

  // noChooseBackoffScore.setDefault( 
  //   math.log( backoffArg/(noChooseBackoff + backoffArg + backoffBoth) )
  // )
  // backoffBothScore.setDefault(
  //   math.log( backoffBoth/(noChooseBackoff + backoffArg + backoffBoth) )
  // )
  // backoffArgScore.setDefault(
  //   math.log( backoffArg/(noChooseBackoff + backoffArg + backoffBoth) )
  // )

  // noStopBackoffScore.setDefault(
  //   math.log( noStopBackoff /(noStopBackoff + stopBackoff) )
  // )
  // stopBackoffScore.setDefault(
  //   math.log( stopBackoff /(noStopBackoff + stopBackoff) )
  // )

  override def associatedGrammar = new DMVBayesianBackoffGrammar(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffHead,
    backoffArg,
    backoffBoth,
    noStopBackoffScore,
    stopBackoffScore,
    noChooseBackoffScore,
    backoffArgScore,
    backoffHeadScore,
    backoffBothScore
  )

  def associatedGrammar(
    newNoStopBackoffScore:AbstractLog1dTable[StopOrNot],
    newStopBackoffScore:AbstractLog1dTable[StopOrNot],
    newNoChooseBackoffScore:AbstractLog1dTable[ChooseArgument],
    newBackoffHeadScore:AbstractLog1dTable[ChooseArgument],
    newBackoffArgScore:AbstractLog1dTable[ChooseArgument],
    newBackoffBothScore:AbstractLog1dTable[ChooseArgument]
  ):AbstractDMVGrammar = new DMVBayesianBackoffGrammar(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffHead,
    backoffArg,
    backoffBoth,
    newNoStopBackoffScore,
    newStopBackoffScore,
    newNoChooseBackoffScore,
    newBackoffHeadScore,
    newBackoffArgScore,
    newBackoffBothScore
  )

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    // println( "order counts is:\n" + orderCounts )
    // println( "stopcounts is:\n" + stopCounts )
    // println( "chooseCounts is:\n" + chooseCounts )

    // println( "\n\n\n[[[====================]]]\n\n\n")

    // We'll be incrementing these throughout the function as we are going to compute each element
    // of Kurihara and Sato (2006), eqn 8 anyway. The first element of eqn 8 has already been
    // computed for us and is totalScore (assuming this is the partial counts that's been collected
    // from VanillaDMV.computePartialCounts).
    // var freeEnergyElementOne = -1 * totalScore
    // var freeEnergyElementTwo = 0D
    // var freeEnergyElementThree = 0D
    // var freeEnergyElementFour = 0D


    // First, compute new interpolation parameters, starting with stop
    val stopNoBackoffInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    val stopBackoffInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    val stopBackoffInterpolationDenom = new Log1dTable( Set[StopOrNot]() )
    val stopBackoffInterpolationTypeCounts = new Log1dTable( Set[StopOrNot]() )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffTypeCounts =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    //println( "Summing over stopCounts to get counts for interpolation parameters and each backoff distribution." )
    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1,h2) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffStopKey = StopOrNot(Word(h2), stopKey.dir, /*true*/ stopKey.adj )
            val backoffHead = Word(h2)

            val thisNoBackoffComponent =
              stopCounts( stopKey, dec ) + noStopBackoffScore( stopKey /*stopKey.w*/ )
            val thisBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey /*stopKey.w*/ )

            // for interpolation parameters, sum out backoff set
            stopNoBackoffInterpolationSums.setValue(
              //stopKey.w,
              stopKey,
              logSum(
                stopNoBackoffInterpolationSums( stopKey /*stopKey.w*/ ),
                thisNoBackoffComponent
              )
            )
            // Use backoff head because our interpolation rules are tied
            stopBackoffInterpolationSums.setValue(
              //backoffHead,
              backoffStopKey,
              logSum(
                stopBackoffInterpolationSums( backoffStopKey /*backoffHead*/ ),
                thisBackoffComponent
              )
            )
            stopBackoffInterpolationTypeCounts.setValue(
              //backoffHead,
              backoffStopKey,
              logSum(
                stopBackoffInterpolationTypeCounts( backoffStopKey /*backoffHead*/ ), 0D
              )
            )
            stopBackoffInterpolationDenom.setValue(
              //stopKey.w,
              stopKey,
              logSum(
                stopBackoffInterpolationDenom( stopKey /*stopKey.w*/ ),
                thisNoBackoffComponent
              )
            )

            // for backoff distribution, only sum over elements of the backoff set.
            stopNoBackoffCounts.setValue(
              stopKey,
              dec,
              logSum(
                stopNoBackoffCounts( stopKey, dec ),
                thisNoBackoffComponent
              )
            )
            stopBackoffCounts.setValue(
              backoffStopKey,
              dec,
              logSum(
                stopBackoffCounts( backoffStopKey, dec ),
                thisBackoffComponent
              )
            )
            stopBackoffTypeCounts.setValue(
              backoffStopKey,
              dec,
              logSum(
                stopBackoffTypeCounts(backoffStopKey,dec), 0D
              )
            )

          }
        }
        case rootHead:AbstractRoot => { /* intentionally empty */ }
      }
    }

    val rootStopCounts =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
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

    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      NotStop,
      0D
    )
    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      Stop,
      Double.NegativeInfinity
    )

    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )
    rootStopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      Stop,
      0D
    )

    // bring tied rules to average:
    stopBackoffInterpolationTypeCounts.domain.foreach{ backoffHead =>
      stopBackoffInterpolationSums.setValue(
        backoffHead,
        stopBackoffInterpolationSums( backoffHead ) -
          stopBackoffInterpolationTypeCounts( backoffHead )
      )
    }
    stopBackoffCounts.parents.foreach{ backoffHead =>
      dmv.stopDecision.foreach{ dec =>
        stopBackoffCounts.setValue(
          backoffHead,
          dec,
          stopBackoffCounts( backoffHead, dec ) -
            stopBackoffTypeCounts( backoffHead, dec )
        )
      }
    }


    //println( "Finishing up sums for stop denominator." )
    stopBackoffInterpolationDenom.domain.foreach{ denom =>
      val WordPair( h1, h2 ) = denom.w
      val backoffHeadWord = Word( h2 )
      val backoffHead = StopOrNot( backoffHeadWord, denom.dir, denom.adj )
      stopBackoffInterpolationDenom.setValue(
        denom,
        logSum(
          Seq(
            stopBackoffInterpolationDenom( denom ),
            stopBackoffInterpolationSums( backoffHead ),
            math.log( stopBackoff + noStopBackoff )
          )
        )
      )
    }

    val newNoStopBackoffScore = new Log1dTable( Set[StopOrNot]() )
    val newStopBackoffScore = new Log1dTable( Set[StopOrNot]() )

    // println( "\n\nstopNoBackoffInterpolationSums:\n" + stopNoBackoffInterpolationSums )
    // println( "\n\nstopBackoffInterpolationSums:\n" + stopBackoffInterpolationSums + "\n\n---\n\n" )

          // val toCalculateFreeEnergyElementTwoNoBackoffSums = new Log1dTable( Set[ObservedLabel]() )
          // val toCalculateFreeEnergyElementTwoBackoffNumerator = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffDenom = Log1dTable( Set[ObservedLabel]() , 0D )

    //println( "Estimating new stop interpolation distributions." )
    stopBackoffInterpolationDenom.domain.foreach{ h =>

      val WordPair( h1, h2 ) = h.w
      val backoffHeadWord = Word( h2 )
      val backoffHeadKey = StopOrNot( backoffHeadWord, h.dir, h.adj )
      val expDigammaDenom = Math.expDigamma( stopBackoffInterpolationDenom( h ) )

      val thisStopNoBackoffTotal = logSum(
        stopNoBackoffInterpolationSums( h ),
        math.log( noStopBackoff )
      )
      newNoStopBackoffScore.setValue(
        h,
        expDigamma( thisStopNoBackoffTotal) - expDigammaDenom
      )


      val thisStopBackoffTotal = logSum(
        stopBackoffInterpolationSums( backoffHeadKey /*backoffHead*/ ),
        math.log( stopBackoff )
      )
      newStopBackoffScore.setValue(
        h,
        expDigamma(
          thisStopBackoffTotal
        ) - expDigammaDenom
      )

            // val realSpaceNoBackoffTotal = math.exp(thisStopNoBackoffTotal)
            // val realSpaceBackoffTotal = math.exp(thisStopBackoffTotal)

            // // I think lgamma takes its argument in real space, not log space
            // // decision to not backoff has only one child, so we can just add it in here
            // freeEnergyElementTwo += lgamma( realSpaceNoBackoffTotal ) - lgamma( noStopBackoff )

            // // decision to backoff has only many children, so we have to sum up
            // toCalculateFreeEnergyElementTwoBackoffNumerator.setValue(
            //   backoffHead,
            //   toCalculateFreeEnergyElementTwoBackoffNumerator( backoffHead ) + realSpaceBackoffTotal
            // )
            // toCalculateFreeEnergyElementTwoBackoffDenom.setValue(
            //   backoffHead,
            //   toCalculateFreeEnergyElementTwoBackoffDenom( backoffHead ) + stopBackoff
            // )

            // freeEnergyElementThree += lgamma( realSpaceNoBackoffTotal ) - lgamma( noStopBackoff )
            // freeEnergyElementThree += lgamma( realSpaceBackoffTotal ) - lgamma( stopBackoff )

            // freeEnergyElementFour +=
            //   ( realSpaceNoBackoffTotal - noStopBackoff ) * newNoStopBackoffScore( h )
            // freeEnergyElementFour +=
            //   ( realSpaceBackoffTotal - stopBackoff ) * newStopBackoffScore( h )
    }

    val stopBackoffDefaultDenom = expDigamma( math.log( noStopBackoff + stopBackoff ) )
    newNoStopBackoffScore.setDefault(
      expDigamma( math.log( noStopBackoff ) ) -
        stopBackoffDefaultDenom
    )
    newStopBackoffScore.setDefault(
      expDigamma( math.log( stopBackoff ) ) -
        stopBackoffDefaultDenom
    )

          // toCalculateFreeEnergyElementTwoBackoffNumerator.domain.foreach{ backoffHead =>
          //   freeEnergyElementTwo +=
          //     lgamma( toCalculateFreeEnergyElementTwoBackoffNumerator( backoffHead ) ) -
          //       lgamma( toCalculateFreeEnergyElementTwoBackoffDenom( backoffHead ) )
          // }


    // whew, now let's get interpolation parameters for chooseScore

    val chooseBackoffInterpolationDenom = new Log1dTable( Set[ChooseArgument]() )

    val chooseNoBackoffInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    val chooseBackoffArgInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    val chooseBackoffHeadInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    val chooseBackoffBothInterpolationSums = new Log1dTable( Set[ChooseArgument]() )

    val chooseBackoffHeadInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    val chooseBackoffArgInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    val chooseBackoffBothInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )

    val chooseBackoffHeadTypeCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseBackoffArgTypeCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseBackoffBothTypeCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    // along with backoff terms
    val chooseNoBackoffCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    val rootChooseCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    //println( "Summing over chooseCounts to get counts for interpolation parameters and each backoff distribution." )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( h1, h2 ) => {
          val backoffHeadKey = ChooseArgument( Word( h2 ), chooseKey.dir )

          chooseCounts(chooseKey).keySet.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {

                val attachDir = chooseKey.dir
                val interpolationWords = WordQuad( h1, h2, a1, a2 )
                val interpolationKey = ChooseArgument( WordQuad( h1, h2, a1, a2 ), attachDir )

                val interpolationBackoffArg = WordTriple( h1, h2, a2 )
                val interpolationBackoffHead = WordTriple( h2, a1, a2 )
                val interpolationBackoffBoth = WordPair( h2, a2 )

                val interpolationBackoffArgKey =
                  ChooseArgument( interpolationBackoffArg, attachDir )
                val interpolationBackoffHeadKey =
                  ChooseArgument( interpolationBackoffHead, attachDir )
                val interpolationBackoffBothKey =
                  ChooseArgument( interpolationBackoffBoth, attachDir )



                val backoffArg = Word(a2)

                val thisNoBackoffComponent =
                  chooseCounts( chooseKey, arg ) + noChooseBackoffScore( interpolationKey )
                val thisBackoffHeadComponent =
                  chooseCounts( chooseKey, arg ) + backoffHeadScore( interpolationKey )
                val thisBackoffArgComponent =
                  chooseCounts( chooseKey, arg ) + backoffArgScore( interpolationKey )
                val thisBackoffBothComponent =
                  chooseCounts( chooseKey, arg ) + backoffBothScore( interpolationKey )

                chooseNoBackoffInterpolationSums.setValue(
                  interpolationKey,
                  logSum(
                    chooseNoBackoffInterpolationSums( interpolationKey ),
                    thisNoBackoffComponent
                  )
                )

                chooseBackoffHeadInterpolationSums.setValue(
                  interpolationBackoffHeadKey,
                  logSum(
                    chooseBackoffArgInterpolationSums( interpolationBackoffHeadKey ),
                    thisBackoffHeadComponent
                  )
                )
                chooseBackoffHeadInterpolationTypeCounts.setValue(
                  interpolationBackoffHeadKey,
                  logSum(
                    chooseBackoffHeadInterpolationTypeCounts( interpolationBackoffHeadKey), 0D
                  )
                )

                chooseBackoffArgInterpolationSums.setValue(
                  interpolationBackoffArgKey,
                  logSum(
                    chooseBackoffArgInterpolationSums( interpolationBackoffArgKey ),
                    thisBackoffArgComponent
                  )
                )
                chooseBackoffArgInterpolationTypeCounts.setValue(
                  interpolationBackoffArgKey,
                  logSum(
                    chooseBackoffArgInterpolationTypeCounts( interpolationBackoffArgKey), 0D
                  )
                )

                chooseBackoffBothInterpolationSums.setValue(
                  interpolationBackoffBothKey,
                  logSum(
                    chooseBackoffBothInterpolationSums( interpolationBackoffBothKey ),
                    thisBackoffBothComponent
                  )
                )
                chooseBackoffBothInterpolationTypeCounts.setValue(
                  interpolationBackoffBothKey,
                  logSum(
                    chooseBackoffBothInterpolationTypeCounts( interpolationBackoffBothKey ), 0D
                  )
                )

                chooseBackoffInterpolationDenom.setValue(
                  interpolationKey,
                  logSum(
                    chooseBackoffInterpolationDenom( interpolationKey ),
                    thisNoBackoffComponent
                  )
                )


                // Also, count up for each backoff distribution (not the interpolation parameters).
                chooseNoBackoffCounts.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    chooseNoBackoffCounts( chooseKey, arg ),
                    thisNoBackoffComponent
                  )
                )
                chooseBackoffHeadCounts.setValue(
                  backoffHeadKey,
                  arg,
                  logSum(
                    chooseBackoffHeadCounts( backoffHeadKey, arg ),
                    thisBackoffHeadComponent
                  )
                )
                chooseBackoffHeadTypeCounts.setValue(
                  backoffHeadKey,
                  arg,
                  logSum(
                    chooseBackoffHeadTypeCounts( backoffHeadKey, arg ),
                    0D
                  )
                )
                chooseBackoffArgCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    chooseBackoffArgCounts( chooseKey, backoffArg ),
                    thisBackoffArgComponent
                  )
                )
                chooseBackoffArgTypeCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    chooseBackoffArgTypeCounts( chooseKey, backoffArg ),
                    0D
                  )
                )
                chooseBackoffBothCounts.setValue(
                  backoffHeadKey,
                  backoffArg,
                  logSum(
                    chooseBackoffBothCounts( backoffHeadKey, backoffArg ),
                    thisBackoffBothComponent
                  )
                )
                chooseBackoffBothTypeCounts.setValue(
                  backoffHeadKey,
                  backoffArg,
                  logSum(
                    chooseBackoffBothTypeCounts( backoffHeadKey, backoffArg ),
                    0D
                  )
                )

              }
              case rootArg:AbstractRoot => {
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

    // bring tied rules to average:

    chooseBackoffHeadInterpolationSums.domain.foreach{ backoffHeadKey =>
      chooseBackoffHeadInterpolationSums.setValue(
        backoffHeadKey,
        chooseBackoffHeadInterpolationSums( backoffHeadKey ) -
          chooseBackoffHeadInterpolationTypeCounts( backoffHeadKey )
      )
    }
    chooseBackoffArgInterpolationSums.domain.foreach{ backoffArgKey =>
      chooseBackoffArgInterpolationSums.setValue(
        backoffArgKey,
        chooseBackoffArgInterpolationSums( backoffArgKey ) -
          chooseBackoffArgInterpolationTypeCounts( backoffArgKey )
      )
    }
    chooseBackoffBothInterpolationSums.domain.foreach{ backoffBothKey =>
      chooseBackoffBothInterpolationSums.setValue(
        backoffBothKey,
        chooseBackoffBothInterpolationSums( backoffBothKey ) -
          chooseBackoffBothInterpolationTypeCounts( backoffBothKey )
      )
    }



    chooseBackoffHeadCounts.parents.foreach{ backoffHeadKey =>
      chooseBackoffHeadCounts(backoffHeadKey).keySet.foreach{ arg =>
        chooseBackoffHeadCounts.setValue(
          backoffHeadKey,
          arg,
          chooseBackoffHeadCounts( backoffHeadKey, arg ) -
            chooseBackoffHeadTypeCounts( backoffHeadKey, arg )
        )
      }
    }
    chooseBackoffArgCounts.parents.foreach{ backoffArgKey =>
      chooseBackoffArgCounts(backoffArgKey).keySet.foreach{ backedOffArg =>
        chooseBackoffArgCounts.setValue(
          backoffArgKey,
          backedOffArg,
          chooseBackoffArgCounts( backoffArgKey, backedOffArg ) -
            chooseBackoffArgTypeCounts( backoffArgKey, backedOffArg )
        )
      }
    }
    chooseBackoffBothCounts.parents.foreach{ backoffBothKey =>
      chooseBackoffBothCounts(backoffBothKey).keySet.foreach{ backoffArg=>
        chooseBackoffBothCounts.setValue(
          backoffBothKey,
          backoffArg,
          chooseBackoffBothCounts( backoffBothKey, backoffArg) -
            chooseBackoffBothTypeCounts( backoffBothKey, backoffArg)
        )
      }
    }

    //println( "Finishing up sums for choose denominator." )
    chooseBackoffInterpolationDenom.domain.foreach{ denom =>
      val WordQuad( h1, h2, a1, a2 ) = denom.h

      val interpolationBackoffHead = WordTriple( h2, a1, a2 )
      val interpolationBackoffArg = WordTriple( h1, h2, a2 )
      val interpolationBackoffBoth = WordPair( h2, a2 )

      val attachDir = denom.dir

      val interpolationBackoffArgKey =
        ChooseArgument( interpolationBackoffArg, attachDir )
      val interpolationBackoffHeadKey =
        ChooseArgument( interpolationBackoffHead, attachDir )
      val interpolationBackoffBothKey =
        ChooseArgument( interpolationBackoffBoth, attachDir )


      chooseBackoffInterpolationDenom.setValue(
        denom,
        logSum(
          Seq(
            chooseBackoffInterpolationDenom( denom ),
            chooseBackoffArgInterpolationSums( interpolationBackoffArgKey ),
            chooseBackoffHeadInterpolationSums( interpolationBackoffHeadKey ),
            chooseBackoffBothInterpolationSums( interpolationBackoffBothKey ),
            math.log( noChooseBackoff + backoffHead + backoffArg + backoffBoth )
          )
        )
      )
    }

    val newNoChooseBackoffScore = new Log1dTable( Set[ChooseArgument]() )
    val newChooseBackoffHeadScore = new Log1dTable( Set[ChooseArgument]() )
    val newChooseBackoffArgScore = new Log1dTable( Set[ChooseArgument]() )
    val newChooseBackoffBothScore = new Log1dTable( Set[ChooseArgument]() )

          // val toCalculateFreeEnergyElementTwoBackoffArgNumerator = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffArgDenom = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffBothNumerator = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffBothDenom = Log1dTable( Set[ObservedLabel]() , 0D)

    //println( "Estimating new choose interpolation distributions." )
    chooseBackoffInterpolationDenom.domain.foreach{ ha =>
      val WordQuad( h1, h2, a1, a2 ) = ha.h

      val interpolationBackoffHead = WordTriple( h2,a1,a2)
      val interpolationBackoffArg = WordTriple( h1,h2,a2)
      val interpolationBackoffBoth = WordPair( h2, a2 )

      val attachDir = ha.dir

      val backoffArgKey =
        ChooseArgument( interpolationBackoffArg, attachDir )
      val backoffHeadKey =
        ChooseArgument( interpolationBackoffHead, attachDir )
      val backoffBothKey =
        ChooseArgument( interpolationBackoffBoth, attachDir )

      val expDigammaDenom = Math.expDigamma( chooseBackoffInterpolationDenom( ha ) )

      val thisChooseNoBackoffTotal = logSum(
        chooseNoBackoffInterpolationSums( ha ),
        math.log( noChooseBackoff )
      )
      newNoChooseBackoffScore.setValue(
        ha,
        expDigamma(
          thisChooseNoBackoffTotal
        ) - expDigammaDenom
      )

      val thisChooseBackoffHeadTotal = logSum(
        chooseBackoffHeadInterpolationSums( backoffHeadKey ),
        math.log( backoffHead )
      )
      newChooseBackoffHeadScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffHeadTotal
        ) - expDigammaDenom
      )


      val thisChooseBackoffArgTotal = logSum(
        chooseBackoffArgInterpolationSums( backoffArgKey ),
        math.log( backoffArg )
      )
      newChooseBackoffArgScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffArgTotal
        ) - expDigammaDenom
      )

      val thisChooseBackoffBothTotal = logSum(
        chooseBackoffBothInterpolationSums( backoffBothKey ),
        math.log( backoffBoth )
      )

      newChooseBackoffBothScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffBothTotal
        ) - expDigammaDenom
      )

            // val realSpaceNoBackoffTotal = math.exp( thisChooseNoBackoffTotal )
            // val realSpaceBackoffArgTotal = math.exp( thisChooseBackoffArgTotal )
            // val realSpaceBackoffBothTotal = math.exp( thisChooseBackoffBothTotal )

            // freeEnergyElementTwo += lgamma( realSpaceNoBackoffTotal ) - lgamma( noChooseBackoff )

            // toCalculateFreeEnergyElementTwoBackoffArgNumerator.setValue(
            //   backoffArgKey,
            //   toCalculateFreeEnergyElementTwoBackoffArgNumerator( backoffArgKey ) +
            //     realSpaceBackoffArgTotal
            // )
            // toCalculateFreeEnergyElementTwoBackoffArgDenom.setValue(
            //   backoffArgKey,
            //   toCalculateFreeEnergyElementTwoBackoffArgDenom( backoffArgKey ) +
            //     backoffArg
            // )

            // toCalculateFreeEnergyElementTwoBackoffBothNumerator.setValue(
            //   backoffBothKey,
            //   toCalculateFreeEnergyElementTwoBackoffBothNumerator( backoffBothKey ) +
            //     realSpaceBackoffBothTotal
            // )
            // toCalculateFreeEnergyElementTwoBackoffBothDenom.setValue(
            //   backoffBothKey,
            //   toCalculateFreeEnergyElementTwoBackoffBothDenom( backoffBothKey ) +
            //     backoffBoth
            // )

            // freeEnergyElementThree += lgamma( realSpaceNoBackoffTotal ) - lgamma( noStopBackoff )
            // freeEnergyElementThree += lgamma( realSpaceBackoffArgTotal ) - lgamma( backoffArg )
            // freeEnergyElementThree += lgamma( realSpaceBackoffBothTotal ) - lgamma( backoffBoth )

            // freeEnergyElementFour +=
            //   ( realSpaceNoBackoffTotal - noChooseBackoff ) * newNoChooseBackoffScore( ha )
            // freeEnergyElementFour +=
            //   ( realSpaceBackoffArgTotal - backoffArg ) * newChooseBackoffArgScore( ha )
            // freeEnergyElementFour +=
            //   ( realSpaceBackoffBothTotal - backoffBoth ) * newChooseBackoffBothScore( ha )
    }

    val chooseBackoffDefaultDenom = expDigamma(
      math.log( noChooseBackoff + backoffHead + backoffArg + backoffBoth )
    )

    newNoChooseBackoffScore.setDefault(
      expDigamma( math.log( noChooseBackoff ) ) -
        chooseBackoffDefaultDenom
    )
    newChooseBackoffHeadScore.setDefault(
      expDigamma( math.log( backoffHead ) ) -
        chooseBackoffDefaultDenom
    )
    newChooseBackoffArgScore.setDefault(
      expDigamma( math.log( backoffArg ) ) -
        chooseBackoffDefaultDenom
    )
    newChooseBackoffBothScore.setDefault(
      expDigamma( math.log( backoffBoth ) ) -
        chooseBackoffDefaultDenom
    )

          // toCalculateFreeEnergyElementTwoBackoffArgNumerator.domain.foreach{ backoffArg =>
          //   freeEnergyElementTwo +=
          //     lgamma( toCalculateFreeEnergyElementTwoBackoffArgNumerator( backoffArg ) ) -
          //       lgamma( toCalculateFreeEnergyElementTwoBackoffArgDenom( backoffArg ) )
          // }
          // toCalculateFreeEnergyElementTwoBackoffBothNumerator.domain.foreach{ backoffBoth =>
          //   freeEnergyElementTwo +=
          //     lgamma( toCalculateFreeEnergyElementTwoBackoffBothNumerator( backoffBoth ) ) -
          //       lgamma( toCalculateFreeEnergyElementTwoBackoffBothDenom( backoffBoth ) )
          // }


    // Ok, now compute backed-off parameters
    //println( "Estimating new stop distributions with backoff." )

    stopNoBackoffCounts.expDigammaNormalize()
    stopBackoffCounts.expDigammaNormalize()

    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            // back off over valence as well
            val backoffHeadKey = StopOrNot( Word(h2), stopKey.dir, /*true*/ stopKey.adj )
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              logSum(
                newNoStopBackoffScore( stopKey /*stopKey.w*/ ) + stopNoBackoffCounts( stopKey, stopDecision ),
                newStopBackoffScore( stopKey /*stopKey.w*/ ) + stopBackoffCounts( backoffHeadKey, stopDecision )
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


    //println( "Estimating new choose distributions with backoff." )

    //println( "chooseNoBackoffCounts before expDigammaNormalize:\n" + chooseNoBackoffCounts )
    chooseNoBackoffCounts.expDigammaNormalize()
    // println( "chooseNoBackoffCounts after expDigammaNormalize:\n" + chooseNoBackoffCounts )
    // println( "chooseBackoffArgCounts before expDigammaNormalize:\n" + chooseBackoffArgCounts )
    chooseBackoffHeadCounts.expDigammaNormalize()
    chooseBackoffArgCounts.expDigammaNormalize()
    // println( "chooseBackoffArgCounts after expDigammaNormalize:\n" + chooseBackoffArgCounts )
    // println( "chooseBackoffBothCounts before expDigammaNormalize:\n" + chooseBackoffBothCounts )
    chooseBackoffBothCounts.expDigammaNormalize()
    //println( "chooseBackoffBothCounts after expDigammaNormalize:\n" + chooseBackoffBothCounts )
    rootChooseCounts.expDigammaNormalize()

    val chooseDefaults = collection.mutable.Map[ChooseArgument,Double]().withDefaultValue(
      expDigamma( 0 ) - expDigamma( math.log( chooseNoBackoffCounts.parents.size ) )
    )

    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>

      chooseKey.h match {
      case WordPair( h1, h2 ) =>
        val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
        chooseDefaults +=
          chooseKey -> 
            logSum(
              Seq(
                newNoChooseBackoffScore.getDefault +
                  chooseNoBackoffCounts.getDefault( chooseKey ),
                newChooseBackoffHeadScore.getDefault +
                  chooseBackoffHeadCounts.getDefault( backoffHeadKey )
              )
            )
        case rootHead:AbstractRoot => {
          // Special handling to allow only one root.
          if( chooseKey.dir == LeftAttachment )
            chooseDefaults +=
              chooseKey -> rootChooseCounts.getDefault( chooseKey )
          else
            chooseDefaults +=
              chooseKey -> Double.NegativeInfinity
        }
      }

      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {
                val backoffWords = WordQuad( h1, h2, a1, a2 )
                val backoffKey = ChooseArgument( backoffWords, chooseKey.dir )
                val backoffChosenArg = Word(a2)
                // println( "\t\t incrementing " + (chooseKey, arg) + " by " +
                //   math.exp( newNoChooseBackoffScore( backoffKey ) ) + " * " +
                //     math.exp( chooseNoBackoffCounts( chooseKey, arg ) ) + " + " +
                //   math.exp( newChooseBackoffArgScore( backoffKey ) ) + " * " + 
                //     math.exp( chooseBackoffArgCounts( chooseKey, backoffChosenArg ) ) + " + " +
                //   math.exp( newChooseBackoffBothScore( backoffKey ) ) + " * " + 
                //     math.exp( chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg ) )
                // )
                // assert(
                //   logSum(
                //     newNoChooseBackoffScore( backoffKey ),
                //     newChooseBackoffArgScore( backoffKey ),
                //     newChooseBackoffBothScore( backoffKey )
                //   ) <= 0D
                // )
                // assert( newNoChooseBackoffScore( backoffKey ) <= 0D )
                // assert( newChooseBackoffArgScore( backoffKey ) <= 0D )
                // assert( newChooseBackoffBothScore( backoffKey ) <= 0D )
                chooseDefaults +=
                  chooseKey ->
                    logSum(
                      Seq(
                        chooseDefaults( chooseKey ),
                        newChooseBackoffArgScore.getDefault + chooseBackoffArgCounts.getDefault( chooseKey ),
                        newChooseBackoffBothScore.getDefault + chooseBackoffBothCounts.getDefault( backoffHeadKey )
                      )
                    )
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(
                      backedoffChoose( chooseKey, arg ),
                      newNoChooseBackoffScore( backoffKey ) + chooseNoBackoffCounts( chooseKey, arg ),
                      newChooseBackoffHeadScore( backoffKey ) +
                        chooseBackoffHeadCounts( backoffHeadKey, arg ),
                      newChooseBackoffArgScore( backoffKey ) +
                        chooseBackoffArgCounts( chooseKey, backoffChosenArg ),
                      newChooseBackoffBothScore( backoffKey ) +
                        chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg )
                    )
                  )
                )
              }
              case rootArg:AbstractRoot => {
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  Double.NegativeInfinity
                )
              }
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
    backedoffChoose.setDefaultMap( chooseDefaults )

    // println( "Testing choose default in partial counts (" + backedoffChoose.parents.size + "):  " +
    //   backedoffChoose( ChooseArgument( Word( "yugioh" ), RightAttachment ) , Word( "yohoho" ) ) +
    //   "\t" + backedoffChoose( ChooseArgument( WordPair( "CC", "6" ), RightAttachment ) , Word( "yohoho" ) ) +
    //   "\t" + 
    //     backedoffChoose( ChooseArgument( WordPair( "CC", "6" ), RightAttachment ) , WordPair( "IN", "2" ) )
    // )

    // assert(
    //   backedoffStop.parents.forall( stopKey =>
    //     logSum( backedoffStop(stopKey).values.toSeq ) <= 0D
    //   )
    // )
    // assert(
    //   backedoffChoose.parents.forall( chooseKey =>
    //     logSum( backedoffChoose(chooseKey).values.toSeq ) <= 0D
    //   )
    // )

    //println( "normalizing stop backoff distribution." )
    //backedoffStop.expDigammaNormalize()
    //println( "normalizing choose backoff distribution." )
    //backedoffChoose.expDigammaNormalize()

    println( "Done!" )

    // val toReturn = new DMVBayesianBackoffPartialCounts(
    //   noChooseBackoff,
    //   backoffArg,
    //   backoffBoth,
    //   noStopBackoff,
    //   stopBackoff,
    //   newNoChooseBackoffScore,
    //   newChooseBackoffArgScore,
    //   newChooseBackoffBothScore,
    //   newNoStopBackoffScore,
    //   newStopBackoffScore
    // )

    //println( "about to create new grammar with newNoStopBackoffScore:\n" + newNoStopBackoffScore )
    val toReturn = associatedGrammar

    // val freeEnergy =
    //   freeEnergyElementOne + freeEnergyElementTwo - freeEnergyElementThree + freeEnergyElementFour

    // println( "Free energy is: " +
    //   freeEnergyElementOne + " + " + freeEnergyElementTwo + " - " + freeEnergyElementThree + " + " +
    //   freeEnergyElementFour + " = " + freeEnergy )

    toReturn.setParams(
      DMVBayesianBackoffParameters(
        // freeEnergy,
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        newNoStopBackoffScore,
        newStopBackoffScore,
        newNoChooseBackoffScore,
        newChooseBackoffArgScore,
        newChooseBackoffHeadScore,
        newChooseBackoffBothScore
      )
    )

    toReturn

  }

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoStopBackoffAlpha: " + noStopBackoff + "\n" +
      "\tstopBackoffAlpha: " + stopBackoff + "\n" +
      "\tnoChooseBackoffAlpha: " + noChooseBackoff + "\n" +
      "\tbackoffHeadAlpha: " + backoffHead + "\n" +
      "\tbackoffArgAlpha: " + backoffArg + "\n" +
      "\tbackoffBothAlpha: " + backoffBoth + "\n"


}

