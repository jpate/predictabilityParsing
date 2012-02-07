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
 *  Finally, I apaologize for the huge number of parameters in the class constructor...
 *
 */
class DMVBayesianBackoffPartialCounts(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noStopBackoff:Double = 35,
  stopBackoff:Double = 70,
  noChooseBackoff:Double = 30,
  backoffArg:Double = 60,
  backoffBoth:Double = 120,
  // these are specific backoff parameters
  noStopBackoffScore:AbstractLog1dTable[ObservedLabel],
  stopBackoffScore:AbstractLog1dTable[ObservedLabel],
  noChooseBackoffScore:AbstractLog1dTable[ObservedLabel],
  backoffArgScore:AbstractLog1dTable[ObservedLabel],
  backoffBothScore:AbstractLog1dTable[ObservedLabel]
) extends DMVPartialCounts {


  def this(
    noStopBackoff:Double,
    stopBackoff:Double,
    noChooseBackoff:Double,
    backoffArg:Double,
    backoffBoth:Double
  ) = this(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    // these are specific backoff parameters
    noStopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( noStopBackoff ) ) -
        Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    ),
    stopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( stopBackoff ) ) -
        Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    ),
    noChooseBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( noChooseBackoff ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffArgScore = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffArg ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffBothScore = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffBoth ) ) -
        Math.expDigamma( math.log(noChooseBackoff + backoffArg + backoffBoth) )
    )
  )
  def this() = this( 35, 70, 30, 60, 120 )

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
    backoffArg,
    backoffBoth,
    noStopBackoffScore,
    stopBackoffScore,
    noChooseBackoffScore,
    backoffArgScore,
    backoffBothScore
  )

  def associatedGrammar(
    newNoStopBackoffScore:AbstractLog1dTable[ObservedLabel],
    newStopBackoffScore:AbstractLog1dTable[ObservedLabel],
    newNoChooseBackoffScore:AbstractLog1dTable[ObservedLabel],
    newBackoffArgScore:AbstractLog1dTable[ObservedLabel],
    newBackoffBothScore:AbstractLog1dTable[ObservedLabel]
  ):AbstractDMVGrammar = new DMVBayesianBackoffGrammar(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    newNoStopBackoffScore,
    newStopBackoffScore,
    newNoChooseBackoffScore,
    newBackoffArgScore,
    newBackoffBothScore
  )

      // // override val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

      // // override val chooseBackoffArgCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
      // // override val chooseBackoffBothCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

      // /*
      // override def incrementStopCounts( stopKey:StopOrNot, decision:StopDecision, increment:Double ) {
      //   //val stopBackoffTotal = Math.sumLogProb(noStopBackoff( stopKey.w ), stopBackoff( stopKey.w ))

      //   stopKey.w match {
      //     case WordPair( h1, h2 ) => {
      //       stopCounts.setValue(
      //         stopKey,
      //         decision,
      //         Math.sumLogProb(
      //           stopCounts( stopKey, decision ),
      //           noStopBackoffScore( stopKey.w ) + increment
      //         )
      //       )

      //       stopBackoffCounts.setValue(
      //         //StopOrNot( Word(h2), stopKey.dir, stopKey.adj ),
      //         stopKey,
      //         decision,
      //         Math.sumLogProb(
      //           stopBackoffCounts( stopKey, decision ),
      //           stopBackoffScore( stopKey.w ) + increment
      //         )
      //       )
      //     }
      //     case root:AbstractRoot =>
      //       stopCounts.setValue(
      //         stopKey,
      //         decision,
      //         Math.sumLogProb(
      //           stopCounts( stopKey, decision ),
      //           increment
      //         )
      //       )
      //   }
      // }
      // */

      // /*
      // override def incrementChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, increment:Double ) {
      //   chooseKey.h match {
      //     case WordPair(h1, h2) => {
      //       arg match {
      //         case WordPair( a1, a2 ) => {
      //           //val backoffBothKey = ChooseArgument( Word(h2), chooseKey.dir )
      //           val backoffWordQuadKey = WordQuad( h1, h2, a1, a2 )
      //           //val backoffArgKey = Word( a2 )

      //           chooseCounts.setValue(
      //             chooseKey,
      //             arg,
      //             Math.sumLogProb(
      //               chooseCounts( chooseKey, arg ),
      //               noChooseBackoffScore( backoffWordQuadKey ) + increment
      //             )
      //           )
      //           chooseBackoffArgCounts.setValue(
      //             chooseKey,
      //             //backoffArgKey,
      //             arg,
      //             Math.sumLogProb(
      //               //chooseBackoffArgCounts(chooseKey,backoffArgKey),
      //               chooseBackoffArgCounts(chooseKey,arg),
      //               backoffArgScore( backoffWordQuadKey ) + increment
      //             )
      //           )
      //           chooseBackoffBothCounts.setValue(
      //             //backoffBothKey,
      //             chooseKey,
      //             //backoffArgKey,
      //             arg,
      //             Math.sumLogProb(
      //               //chooseBackoffBothCounts(backoffBothKey,backoffArgKey),
      //               chooseBackoffBothCounts(chooseKey,arg),
      //               backoffBothScore( backoffWordQuadKey ) + increment
      //             )
      //           )
      //         }
      //         case rootArg:AbstractRoot => {
      //           chooseCounts.setValue(
      //             chooseKey,
      //             arg,
      //             Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
      //           )
      //         }
      //       }
      //     }
      //     case rootHead:AbstractRoot => {
      //       chooseCounts.setValue(
      //         chooseKey,
      //         arg,
      //         Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
      //       )
      //     }
      //   }
      //   // chooseCounts.setValue(
      //   //   chooseKey,
      //   //   arg,
      //   //   Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
      //   // )
      // }
      // */

      // /*
      // override def destructivePlus( otherCounts:DMVPartialCounts ) {

      //   otherCounts.orderCounts.parents.foreach{ w =>
      //     incrementOrderCounts( w , LeftFirst , otherCounts.orderCounts( w, LeftFirst ) )
      //     incrementOrderCounts( w , RightFirst , otherCounts.orderCounts( w, RightFirst ) )
      //   }


      //   otherCounts.stopCounts.parents.foreach{ stopKey =>
      //     dmv.stopDecision.foreach{ decision =>
      //       stopCounts.setValue(
      //         stopKey,
      //         decision,
      //         Math.sumLogProb(
      //           stopCounts( stopKey, decision ),
      //           otherCounts.stopCounts( stopKey, decision )
      //         )
      //       )
      //     }
      //   }

      //   otherCounts.stopBackoffCounts.parents.foreach{ stopKey =>
      //     dmv.stopDecision.foreach{ decision =>
      //       stopBackoffCounts.setValue(
      //         stopKey,
      //         decision,
      //         Math.sumLogProb(
      //           stopBackoffCounts( stopKey, decision ),
      //           otherCounts.stopBackoffCounts( stopKey, decision )
      //         )
      //       )
      //     }
      //   }


      //   otherCounts.chooseCounts.parents.foreach{ chooseKey =>
      //     otherCounts.chooseCounts(chooseKey).keySet.foreach{ w =>
      //       chooseCounts.setValue(
      //         chooseKey,
      //         w,
      //         Math.sumLogProb(
      //           chooseCounts( chooseKey, w ),
      //           otherCounts.chooseCounts( chooseKey, w )
      //         )
      //       )
      //     }
      //   }

      //   otherCounts.chooseBackoffArgCounts.parents.foreach{ chooseKey =>
      //     otherCounts.chooseBackoffArgCounts(chooseKey).keySet.foreach{ w =>
      //       chooseBackoffArgCounts.setValue(
      //         chooseKey,
      //         w,
      //         Math.sumLogProb(
      //           chooseBackoffArgCounts( chooseKey, w ),
      //           otherCounts.chooseBackoffArgCounts( chooseKey, w )
      //         )
      //       )
      //     }
      //   }

      //   otherCounts.chooseBackoffBothCounts.parents.foreach{ chooseKey =>
      //     otherCounts.chooseBackoffBothCounts(chooseKey).keySet.foreach{ w =>
      //       chooseBackoffBothCounts.setValue(
      //         chooseKey,
      //         w,
      //         Math.sumLogProb(
      //           chooseBackoffBothCounts( chooseKey, w ),
      //           otherCounts.chooseBackoffBothCounts( chooseKey, w )
      //         )
      //       )
      //     }
      //   }

      //   // Technically we should be using the free energy, but I don't want to deal with it yet.
      //   multiplyTotalScore( otherCounts.getTotalScore )
      // }
      // */

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    // We'll be incrementing this throughout the function as we are going to compute each element of
    // Kurihara and Sato (2006), eqn 8 anyway. The first element of eqn 8 has already been computed
    // for us and is totalScore (assuming this is the partial counts that's been collected from
    // VanillaDMV.computePartialCounts).
    var freeEnergyElementOne = totalScore

    // expDigamma without exp because we are in log-space
    def expDigamma( input:Double ) = {
      import math.{exp,log}
      var r = 0D
      var x = exp( input )
      while( x <= 5 ) {
        r -= 1/x
        x += 1
      }
      val f = 1/(x*x)
      val t = f*(-1D/12.0 + f*(1D/120.0 + f*(-1D/252.0 + f*(1/240.0 + f*(-1/132.0
          + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
      r + log(x) - 0.5/x + t;
    }

    // First, compute new interpolation parameters, starting with stop
    val stopNoBackoffInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    val stopBackoffInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    val stopBackoffInterpolationDenom = new Log1dTable( Set[ObservedLabel]() )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    //println( "Summing over stopCounts to get counts for interpolation parameters and each backoff distribution." )
    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1,h2) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffStopKey = StopOrNot(Word(h2), stopKey.dir, stopKey.adj)
            val backoffHead = Word(h2)

            val thisNoBackoffComponent =
              stopCounts( stopKey, dec ) + noStopBackoffScore( stopKey.w )
            val thisBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey.w )

            // for interpolation parameters, sum over everything except head word.
            stopNoBackoffInterpolationSums.setValue(
              stopKey.w,
              logSum(
                stopNoBackoffInterpolationSums( stopKey.w ),
                thisNoBackoffComponent
              )
            )
            // Use backoff head because our interpolation rules are tied
            stopBackoffInterpolationSums.setValue(
              // backoffHead,
              stopKey.w,
              logSum(
                //stopBackoffInterpolationSums( backoffHead ),
                stopBackoffInterpolationSums( stopKey.w ),
                thisBackoffComponent
              )
            )
            stopBackoffInterpolationDenom.setValue(
              stopKey.w,
              logSum(
                stopBackoffInterpolationDenom( stopKey.w ),
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

          }
        }
        case rootHead:AbstractRoot => { /* intentionally empty */ }
      }
    }
    //println( "Finishing up sums for stop denominator." )
    stopBackoffInterpolationDenom.domain.foreach{ denom =>
      val WordPair( h1, h2 ) = denom
      stopBackoffInterpolationDenom.setValue(
        denom,
        logSum(
          Seq(
            stopBackoffInterpolationDenom( denom ),
            //stopBackoffInterpolationSums( Word( h2 ) ),
            stopBackoffInterpolationSums( denom ),
            math.log( stopBackoff + noStopBackoff )
          )
        )
      )
    }

    val newNoStopBackoffScore = new Log1dTable( Set[ObservedLabel]() )
    val newStopBackoffScore = new Log1dTable( Set[ObservedLabel]() )

    //println( "Estimating new stop interpolation distributions." )
    stopBackoffInterpolationDenom.domain.foreach{ h =>

      val WordPair( h1, h2 ) = h
      val backoffHead = Word( h2 )

      val thisStopNoBackoffTotal = logSum(
        stopNoBackoffInterpolationSums( h ),
        math.log( noStopBackoff )
      )
      newNoStopBackoffScore.setValue(
        h,
        expDigamma( thisStopNoBackoffTotal) - expDigamma( stopBackoffInterpolationDenom( h ) )
      )

      val thisStopBackoffTotal = logSum(
        //stopBackoffInterpolationSums( backoffHead ),
        stopBackoffInterpolationSums( h ),
        math.log( stopBackoff )
      )
      newStopBackoffScore.setValue(
        h,
        expDigamma(
          thisStopBackoffTotal
        ) - expDigamma( stopBackoffInterpolationDenom( h ) )
      )

    }


    // whew, now let's get interpolation parameters for chooseScore

    val chooseBackoffInterpolationDenom = new Log1dTable( Set[ObservedLabel]() )
    val chooseNoBackoffInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    val chooseBackoffArgInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    val chooseBackoffBothInterpolationSums = new Log1dTable( Set[ObservedLabel]() )

    // along with backoff terms
    val chooseNoBackoffCounts =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    //println( "Summing over chooseCounts to get counts for interpolation parameters and each backoff distribution." )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( h1, h2 ) => {
          val backoffBothKey = ChooseArgument( Word( h2 ), chooseKey.dir )

          chooseCounts(chooseKey).keySet.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {

                val interpolationKey = WordQuad( h1, h2, a1, a2 )
                val interpolationBackoffArgKey = WordTriple( h1, h2, a2 )
                val interpolationBackoffBothKey = WordPair( h2, a2 )
                val backoffArg = Word(a2)

                val thisNoBackoffComponent =
                  chooseCounts( chooseKey, arg ) + noChooseBackoffScore( interpolationKey )
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

                chooseBackoffArgInterpolationSums.setValue(
                  //interpolationBackoffArgKey,
                  interpolationKey,
                  logSum(
                    //chooseBackoffArgInterpolationSums( interpolationBackoffArgKey ),
                    chooseBackoffArgInterpolationSums( interpolationKey ),
                    thisBackoffArgComponent
                  )
                )

                chooseBackoffBothInterpolationSums.setValue(
                  //interpolationBackoffBothKey,
                  interpolationKey,
                  logSum(
                    //chooseBackoffBothInterpolationSums( interpolationBackoffBothKey ),
                    chooseBackoffBothInterpolationSums( interpolationKey ),
                    thisBackoffBothComponent
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
                chooseBackoffArgCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    chooseBackoffArgCounts( chooseKey, backoffArg ),
                    thisBackoffArgComponent
                  )
                )
                chooseBackoffBothCounts.setValue(
                  backoffBothKey,
                  backoffArg,
                  logSum(
                    chooseBackoffBothCounts( backoffBothKey, backoffArg ),
                    thisBackoffBothComponent
                  )
                )

              }
              case rootArg:AbstractRoot => { /* Intentionally Empty */ }
            }
          }
        }
        case rootHead:AbstractRoot => { /* Intentionally Empty */ }
      }
    }


    //println( "Finishing up sums for choose denominator." )
    chooseBackoffInterpolationDenom.domain.foreach{ denom =>
      val WordQuad( h1, h2, a1, a2 ) = denom
      val interpolationBackoffArgKey = WordTriple( h1, h2, a2 )
      val interpolationBackoffBothKey = WordPair( h2, a2 )
      chooseBackoffInterpolationDenom.setValue(
        denom,
        logSum(
          Seq(
            chooseBackoffInterpolationDenom( denom ),
            //chooseBackoffArgInterpolationSums( interpolationBackoffArgKey ),
            chooseBackoffArgInterpolationSums( denom ),
            //chooseBackoffBothInterpolationSums( interpolationBackoffBothKey ),
            chooseBackoffBothInterpolationSums( denom ),
            math.log( noChooseBackoff + backoffArg + backoffBoth )
          )
        )
      )
    }

    val newNoChooseBackoffScore = new Log1dTable( Set[ObservedLabel]() )
    val newChooseBackoffArgScore = new Log1dTable( Set[ObservedLabel]() )
    val newChooseBackoffBothScore = new Log1dTable( Set[ObservedLabel]() )

    //println( "Estimating new choose interpolation distributions." )
    chooseBackoffInterpolationDenom.domain.foreach{ ha =>
      val WordQuad( h1, h2, a1, a2 ) = ha
      val backoffArgKey = WordTriple( h1,h2,a2)
      val backoffBothKey = WordPair( h2, a2 )
      val expDigammaDenom = expDigamma( chooseBackoffInterpolationDenom( ha ) )

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

      val thisChooseBackoffArgTotal = logSum(
        //chooseBackoffArgInterpolationSums( backoffArgKey ),
        chooseBackoffArgInterpolationSums( ha ),
        math.log( backoffArg )
      )
      newChooseBackoffArgScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffArgTotal
        ) - expDigammaDenom
      )

      val thisChooseBackoffBothTotal = logSum(
        //chooseBackoffBothInterpolationSums( backoffBothKey ),
        chooseBackoffBothInterpolationSums( ha ),
        math.log( backoffBoth )
      )
      newChooseBackoffBothScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffBothTotal
        ) - expDigammaDenom
      )

    }


    // Ok, now compute backed-off parameters
    //println( "Estimating new stop distributions with backoff." )
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
                newNoStopBackoffScore(stopKey.w) + stopNoBackoffCounts( stopKey, stopDecision ),
                newStopBackoffScore(stopKey.w) + stopBackoffCounts( backoffHeadKey, stopDecision )
              )
            )
          }
          case rootHead:AbstractRoot => {
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              stopCounts( stopKey, stopDecision )
            )
          }
        }
      }
    }


    //println( "Estimating new choose distributions with backoff." )
    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {
                val backoffKey = WordQuad( h1, h2, a1, a2 )
                val backoffChosenArg = Word(a2)
                // println( "\t\t incrementing " + (chooseKey, arg) + " by " +
                //   math.exp( newNoChooseBackoffScore( backoffKey ) ) + " * " +
                //     math.exp( chooseCounts( chooseKey, arg ) ) + " + " +
                //   math.exp( newChooseBackoffArgScore( backoffKey ) ) + " * " + 
                //     math.exp( chooseBackoffArgCounts( chooseKey, backoffChosenArg ) ) + " + " +
                //   math.exp( newChooseBackoffBothScore( backoffKey ) ) + " * " + 
                //     math.exp( chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg ) )
                // )
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(
                      backedoffChoose( chooseKey, arg ),
                      newNoChooseBackoffScore( backoffKey ) + chooseNoBackoffCounts( chooseKey, arg ),
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
                  chooseCounts( chooseKey, arg )
                )
              }
            }
          }
          case rootHead:AbstractRoot => {
            backedoffChoose.setValue(
              chooseKey,
              arg,
              chooseCounts( chooseKey, arg )
            )
          }
        }
      }
    }

    //println( "normalizing stop backoff distribution." )
    backedoffStop.expDigammaNormalize(0.1)
    //println( "normalizing choose backoff distribution." )
    backedoffChoose.expDigammaNormalize(0.1)

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
    val toReturn = associatedGrammar/*(
      newNoStopBackoffScore,
      newStopBackoffScore,
      newNoChooseBackoffScore,
      newChooseBackoffArgScore,
      newChooseBackoffBothScore
    )*/

    toReturn.setParams(
      DMVBayesianBackoffParameters(
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        newNoStopBackoffScore,
        newStopBackoffScore,
        newNoChooseBackoffScore,
        newChooseBackoffArgScore,
        newChooseBackoffBothScore
      )
    )

    toReturn

  }


}

