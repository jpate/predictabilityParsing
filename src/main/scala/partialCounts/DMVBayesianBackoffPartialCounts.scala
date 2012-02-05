package predictabilityParsing.partialCounts

import scalala.library.Numerics.lgamma
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

  // override val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

  // override val chooseBackoffArgCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  // override val chooseBackoffBothCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  override def incrementStopCounts( stopKey:StopOrNot, decision:StopDecision, increment:Double ) {
    //val stopBackoffTotal = Math.sumLogProb(noStopBackoff( stopKey.w ), stopBackoff( stopKey.w ))

    stopKey.w match {
      case WordPair( h1, h2 ) => {
        stopCounts.setValue(
          stopKey,
          decision,
          Math.sumLogProb(
            stopCounts( stopKey, decision ),
            noStopBackoffScore( stopKey.w ) + increment
          )
        )

        stopBackoffCounts.setValue(
          //StopOrNot( Word(h2), stopKey.dir, stopKey.adj ),
          stopKey,
          decision,
          Math.sumLogProb(
            stopBackoffCounts( stopKey, decision ),
            stopBackoffScore( stopKey.w ) + increment
          )
        )
      }
      case root:AbstractRoot =>
        stopCounts.setValue(
          stopKey,
          decision,
          Math.sumLogProb(
            stopCounts( stopKey, decision ),
            increment
          )
        )
    }
  }

  override def incrementChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, increment:Double ) {

    chooseKey.h match {
      case WordPair(h1, h2) => {
        arg match {
          case WordPair( a1, a2 ) => {
            //val backoffBothKey = ChooseArgument( Word(h2), chooseKey.dir )
            val backoffWordQuadKey = WordQuad( h1, h2, a1, a2 )
            //val backoffArgKey = Word( a2 )

            chooseCounts.setValue(
              chooseKey,
              arg,
              Math.sumLogProb(
                chooseCounts( chooseKey, arg ),
                noChooseBackoffScore( backoffWordQuadKey ) + increment
              )
            )
            chooseBackoffArgCounts.setValue(
              chooseKey,
              //backoffArgKey,
              arg,
              Math.sumLogProb(
                //chooseBackoffArgCounts(chooseKey,backoffArgKey),
                chooseBackoffArgCounts(chooseKey,arg),
                backoffArgScore( backoffWordQuadKey ) + increment
              )
            )
            chooseBackoffBothCounts.setValue(
              //backoffBothKey,
              chooseKey,
              //backoffArgKey,
              arg,
              Math.sumLogProb(
                //chooseBackoffBothCounts(backoffBothKey,backoffArgKey),
                chooseBackoffBothCounts(chooseKey,arg),
                backoffBothScore( backoffWordQuadKey ) + increment
              )
            )
          }
          case rootArg:AbstractRoot => {
            chooseCounts.setValue(
              chooseKey,
              arg,
              Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
            )
          }
        }
      }
      case rootHead:AbstractRoot => {
        chooseCounts.setValue(
          chooseKey,
          arg,
          Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
        )
      }
    }
    // chooseCounts.setValue(
    //   chooseKey,
    //   arg,
    //   Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
    // )
  }

  override def destructivePlus( otherCounts:DMVPartialCounts ) {

    otherCounts.orderCounts.parents.foreach{ w =>
      incrementOrderCounts( w , LeftFirst , otherCounts.orderCounts( w, LeftFirst ) )
      incrementOrderCounts( w , RightFirst , otherCounts.orderCounts( w, RightFirst ) )
    }


    otherCounts.stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ decision =>
        stopCounts.setValue(
          stopKey,
          decision,
          Math.sumLogProb(
            stopCounts( stopKey, decision ),
            otherCounts.stopCounts( stopKey, decision )
          )
        )
      }
    }

    otherCounts.stopBackoffCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ decision =>
        stopBackoffCounts.setValue(
          stopKey,
          decision,
          Math.sumLogProb(
            stopBackoffCounts( stopKey, decision ),
            otherCounts.stopBackoffCounts( stopKey, decision )
          )
        )
      }
    }


    otherCounts.chooseCounts.parents.foreach{ chooseKey =>
      otherCounts.chooseCounts(chooseKey).keySet.foreach{ w =>
        chooseCounts.setValue(
          chooseKey,
          w,
          Math.sumLogProb(
            chooseCounts( chooseKey, w ),
            otherCounts.chooseCounts( chooseKey, w )
          )
        )
      }
    }

    otherCounts.chooseBackoffArgCounts.parents.foreach{ chooseKey =>
      otherCounts.chooseBackoffArgCounts(chooseKey).keySet.foreach{ w =>
        chooseBackoffArgCounts.setValue(
          chooseKey,
          w,
          Math.sumLogProb(
            chooseBackoffArgCounts( chooseKey, w ),
            otherCounts.chooseBackoffArgCounts( chooseKey, w )
          )
        )
      }
    }

    otherCounts.chooseBackoffBothCounts.parents.foreach{ chooseKey =>
      otherCounts.chooseBackoffBothCounts(chooseKey).keySet.foreach{ w =>
        chooseBackoffBothCounts.setValue(
          chooseKey,
          w,
          Math.sumLogProb(
            chooseBackoffBothCounts( chooseKey, w ),
            otherCounts.chooseBackoffBothCounts( chooseKey, w )
          )
        )
      }
    }

    // Technically we should be using the free energy, but I don't want to deal with it yet.
    multiplyTotalScore( otherCounts.getTotalScore )
  }

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
    val stopNoBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    val stopBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    val stopBackoffDenom = new Log1dTable( Set[ObservedLabel]() )

    // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopBackoffDistributionSums =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1,h2) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffStopKey = StopOrNot(Word(h2), stopKey.dir, stopKey.adj)
            stopNoBackoffSums.setValue(
              stopKey.w,
              Math.sumLogProb(
                stopNoBackoffSums( stopKey.w ),
                stopCounts( stopKey, dec )
              )
            )
            stopBackoffSums.setValue(
              stopKey.w,
              Math.sumLogProb(
                stopBackoffSums( stopKey.w ),
                //stopBackoffCounts( backoffStopKey, dec )
                stopBackoffCounts( stopKey, dec )
              )
            )

            stopBackoffDistributionSums.setValue(
              backoffStopKey,
              dec,
              Math.sumLogProb(
                stopBackoffDistributionSums( backoffStopKey, dec ),
                stopBackoffCounts( stopKey, dec )
              )
            )

            stopBackoffDenom.setValue(
              stopKey.w,
              List(
                stopBackoffDenom( stopKey.w ),
                stopCounts( stopKey, dec ),
                //stopBackoffCounts( backoffStopKey, dec )
                stopBackoffCounts( stopKey, dec )
              ).reduce(Math.sumLogProb(_,_))
            )
          }
        }
        case rootHead:AbstractRoot => {
          // intentionally empty
        }
      }
    }
    stopBackoffDenom.domain.foreach{ denom =>
      stopBackoffDenom.setValue(
        denom,
        Math.sumLogProb(
          stopBackoffDenom( denom ),
          math.log( stopBackoff + noStopBackoff )
        )
      )
    }

    val newNoStopBackoffScore = new Log1dTable( Set[ObservedLabel]() )
    val newStopBackoffScore = new Log1dTable( Set[ObservedLabel]() )

    var freeEnergyElementTwo = 0D
    var freeEnergyElementThree = 0D
    var freeEnergyElementFour = 0D
    stopBackoffDenom.domain.foreach{ h =>

      val thisStopNoBackoffTotal = Math.sumLogProb(
        stopNoBackoffSums( h ),
        math.log( noStopBackoff )
      )
      newNoStopBackoffScore.setValue(
        h,
        expDigamma( thisStopNoBackoffTotal) - expDigamma( stopBackoffDenom( h ) )
      )

      val thisStopBackoffTotal = Math.sumLogProb(
        stopBackoffSums( h ),
        math.log( noStopBackoff )
      )
      newStopBackoffScore.setValue(
        h,
        expDigamma(
          thisStopBackoffTotal
        ) - expDigamma( stopBackoffDenom( h ) )
      )

      // Also, let's compute elements 2, 3, and 4 of the free energy formula.
      freeEnergyElementTwo +=
        lgamma( thisStopNoBackoffTotal + thisStopBackoffTotal ) -
          lgamma( noStopBackoff + stopBackoff )

      freeEnergyElementThree +=
        ( lgamma( thisStopNoBackoffTotal ) - lgamma( noStopBackoff ) ) +
        ( lgamma( thisStopBackoffTotal ) - lgamma( stopBackoff ) )

      freeEnergyElementFour +=
        ( thisStopNoBackoffTotal - noStopBackoff ) * newNoStopBackoffScore( h ) +
        ( thisStopBackoffTotal - stopBackoff ) * newStopBackoffScore( h )

    }


    // whew, now let's get interpolation parameters for chooseScore

    val chooseBackoffDenom = new Log1dTable( Set[ObservedLabel]() )
    val chooseNoBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    val chooseBackoffArgSums = new Log1dTable( Set[ObservedLabel]() )
    val chooseBackoffArgDistributionSums =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val chooseBackoffBothSums = new Log1dTable( Set[ObservedLabel]() )
    val chooseBackoffBothDistributionSums =
      new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( h1, h2 ) => {
          val backoffBothKey = ChooseArgument( Word( h2 ), chooseKey.dir )
          chooseCounts(chooseKey).keySet.par.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {
                val sumsKey = WordQuad( h1, h2, a1, a2 )
                val backoffArg = Word(a2)
                chooseNoBackoffSums.setValue(
                  sumsKey,
                  Math.sumLogProb(
                    chooseNoBackoffSums( sumsKey ),
                    chooseCounts( chooseKey, arg )
                  )
                )

                chooseBackoffArgSums.setValue(
                  sumsKey,
                  Math.sumLogProb(
                    chooseBackoffArgSums( sumsKey ),
                    //chooseBackoffArgCounts( chooseKey, backoffArg )
                    chooseBackoffArgCounts( chooseKey, arg )
                  )
                )
                chooseBackoffArgDistributionSums.setValue(
                  chooseKey,
                  backoffArg,
                  Math.sumLogProb(
                    chooseBackoffArgDistributionSums( chooseKey, backoffArg ),
                    //chooseBackoffArgCounts( chooseKey, backoffArg )
                    chooseBackoffArgCounts( chooseKey, arg )
                  )
                )

                chooseBackoffBothSums.setValue(
                  sumsKey,
                  Math.sumLogProb(
                    chooseBackoffBothSums( sumsKey ),
                    //chooseBackoffBothCounts( backoffBothKey, backoffArg )
                    chooseBackoffBothCounts( chooseKey, arg )
                  )
                )
                chooseBackoffBothDistributionSums.setValue(
                  backoffBothKey,
                  backoffArg,
                  Math.sumLogProb(
                    chooseBackoffBothDistributionSums( backoffBothKey, backoffArg ),
                    //chooseBackoffBothCounts( backoffBothKey, backoffArg )
                    chooseBackoffBothCounts( chooseKey, arg )
                  )
                )

                chooseBackoffDenom.setValue(
                  sumsKey,
                  //List(
                  Math.sumLogProb(
                    chooseBackoffDenom( sumsKey ),
                    Math.sumLogProb(
                      chooseCounts( chooseKey, arg ),
                      Math.sumLogProb(
                        //chooseBackoffArgCounts( chooseKey, backoffArg ),
                        chooseBackoffArgCounts( chooseKey, arg ),
                        //chooseBackoffBothCounts( backoffBothKey, backoffArg )
                        chooseBackoffBothCounts( chooseKey, arg )
                      )
                    )
                  )
                  //).reduce( Math.sumLogProb(_,_) )
                )
              }
              case rootArg:AbstractRoot => { /* Intentionally Empty */ }
            }
          }
        }
        case rootHead:AbstractRoot => { /* Intentionally Empty */ }
      }
    }


    chooseBackoffDenom.domain.foreach{ denom =>
      chooseBackoffDenom.setValue(
        denom,
        Math.sumLogProb(
          chooseBackoffDenom( denom ),
          math.log( noChooseBackoff + backoffArg + backoffBoth )
        )
      )
    }

    val newNoChooseBackoffScore = new Log1dTable( Set[ObservedLabel]() )
    val newChooseBackoffArgScore = new Log1dTable( Set[ObservedLabel]() )
    val newChooseBackoffBothScore = new Log1dTable( Set[ObservedLabel]() )

    chooseBackoffDenom.domain.foreach{ ha =>
      val expDigammaDenom = expDigamma( chooseBackoffDenom( ha ) )

      val thisChooseNoBackoffTotal = Math.sumLogProb(
        chooseNoBackoffSums( ha ),
        math.log( noChooseBackoff )
      )
      newNoChooseBackoffScore.setValue(
        ha,
        expDigamma(
          thisChooseNoBackoffTotal
        ) - expDigammaDenom
      )

      val thisChooseBackoffArgTotal = Math.sumLogProb(
        chooseBackoffArgSums( ha ),
        math.log( backoffArg )
      )
      newChooseBackoffArgScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffArgTotal
        ) - expDigammaDenom
      )

      val thisChooseBackoffBothTotal = Math.sumLogProb(
        chooseBackoffBothSums( ha ),
        math.log( backoffBoth )
      )
      newChooseBackoffBothScore.setValue(
        ha,
        expDigamma(
          thisChooseBackoffBothTotal
        ) - expDigammaDenom
      )

      // Also, let's compute elements 2, 3 and 4 of the free energy formula.
      freeEnergyElementTwo +=
        lgamma( thisChooseNoBackoffTotal + thisChooseBackoffArgTotal + thisChooseBackoffBothTotal ) -
          lgamma( noChooseBackoff + backoffArg + backoffBoth )

      freeEnergyElementThree +=
        ( lgamma( thisChooseNoBackoffTotal ) - lgamma( noChooseBackoff ) ) +
        ( lgamma( thisChooseBackoffArgTotal ) - lgamma( backoffArg ) ) +
        ( lgamma( thisChooseBackoffBothTotal ) - lgamma( backoffBoth ) )

      freeEnergyElementFour +=
        ( thisChooseNoBackoffTotal - noChooseBackoff ) * newNoChooseBackoffScore( ha ) +
        ( thisChooseBackoffArgTotal - backoffArg ) * newChooseBackoffArgScore( ha ) +
        ( thisChooseBackoffBothTotal - backoffBoth ) * newChooseBackoffBothScore( ha )
    }


    // Ok, now compute backed-off parameters
    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = StopOrNot( Word(h2), stopKey.dir, stopKey.adj )
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              Math.sumLogProb(
                newNoStopBackoffScore(stopKey.w) + stopCounts( stopKey, stopDecision ),
                // newStopBackoffScore(stopKey.w) + stopBackoffCounts( backoffHeadKey, stopDecision )
                newStopBackoffScore(stopKey.w) + stopBackoffDistributionSums( backoffHeadKey, stopDecision )
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
                //  println( "\t\t incrementing " + (chooseKey, arg) + " by " +
                //    math.exp( newNoChooseBackoffScore( backoffKey ) ) + " * " +
                //      math.exp( chooseCounts( chooseKey, arg ) ) + " + " +
                //    math.exp( newChooseBackoffArgScore( backoffKey ) ) + " * " + 
                //      math.exp( chooseBackoffArgCounts( chooseKey, backoffChosenArg ) ) + " + " +
                //    math.exp( newChooseBackoffBothScore( backoffKey ) ) + " * " + 
                //      math.exp( chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg ) )
                //  )
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  Math.sumLogProb(
                    backedoffChoose( chooseKey, arg ),
                    Math.sumLogProb(
                      newNoChooseBackoffScore( backoffKey ) + chooseCounts( chooseKey, arg ),
                      Math.sumLogProb(
                        newChooseBackoffArgScore( backoffKey ) +
                          chooseBackoffArgDistributionSums( chooseKey, backoffChosenArg ),
                        newChooseBackoffBothScore( backoffKey ) +
                          chooseBackoffBothDistributionSums( backoffHeadKey, backoffChosenArg )
                      )
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

    backedoffStop.expDigammaNormalize()
    //println( "backedoffChoose before expDigammaNormalize:\n" + backedoffChoose )
    backedoffChoose.expDigammaNormalize()
    //println( "backedoffChoose after expDigammaNormalize:\n" + backedoffChoose )

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

