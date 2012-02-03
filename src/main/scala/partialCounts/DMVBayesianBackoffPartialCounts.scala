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
 *
 *  Finally, I apaologize for the huge number of parameters in the class constructor...
 *
 */
class DMVBayesianBackoffPartialCounts(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noChooseBackoff:Double = 30,
  backoffArg:Double = 60,
  backoffBoth:Double = 120,
  noStopBackoff:Double = 35,
  stopBackoff:Double = 70,
  // these are specific backoff parameters
  noChooseBackoffScore:AbstractLog1dTable[ObservedLabel],
  backoffArgScore:AbstractLog1dTable[ObservedLabel],
  backoffBothScore:AbstractLog1dTable[ObservedLabel],
  noStopBackoffScore:AbstractLog1dTable[ObservedLabel],
  stopBackoffScore:AbstractLog1dTable[ObservedLabel]
) extends DMVPartialCounts {

  def this(
    noChooseBackoff:Double,
    backoffArg:Double,
    backoffBoth:Double,
    noStopBackoff:Double,
    stopBackoff:Double
  ) = this(
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    noStopBackoff,
    stopBackoff,
    // these are specific backoff parameters
    noChooseBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( noChooseBackoff/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffArgScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( backoffArg/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffBothScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( backoffBoth/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    noStopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( noStopBackoff /(noStopBackoff + stopBackoff) )
    ),
    stopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( stopBackoff /(noStopBackoff + stopBackoff) )
    )
  )
  def this() = this( 30, 60, 120, 35, 70)

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
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    noStopBackoff,
    stopBackoff,
    noChooseBackoffScore,
    backoffArgScore,
    backoffBothScore,
    noStopBackoffScore,
    stopBackoffScore
  )

  val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

  val chooseBackoffArgCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val chooseBackoffBothCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  override def incrementStopCounts( stopKey:StopOrNot, decision:StopDecision, increment:Double ) {
    //val stopBackoffTotal = Math.sumLogProb(noStopBackoff( stopKey.w ), stopBackoff( stopKey.w ))

    stopKey.w match {
      case WordPair( h1, _ ) => {
        stopCounts.setValue(
          stopKey,
          decision,
          Math.sumLogProb(
            stopCounts( stopKey, decision ),
            noStopBackoffScore( stopKey.w ) + increment
          )
        )

        stopBackoffCounts.setValue(
          StopOrNot( Word(h1), stopKey.dir, stopKey.adj ),
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
            val backoffBothKey = ChooseArgument( Word(h1), chooseKey.dir )
            val backoffWordQuadKey = new WordQuad( h1, h2, a1, a2 )
            val backoffArgKey = Word( a1 )

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
              backoffArgKey,
              Math.sumLogProb(
                chooseBackoffArgCounts(chooseKey,arg),
                backoffArgScore( backoffWordQuadKey ) + increment
              )
            )
            chooseBackoffBothCounts.setValue(
              backoffBothKey,
              backoffArgKey,
              Math.sumLogProb(
                chooseBackoffArgCounts(chooseKey,arg),
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

  def destructivePlus( otherCounts:DMVBayesianBackoffPartialCounts ) {
    println( "DMVBayesianBackoffPartialCounts destructivePlus" )

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
    // println( "First, stop interpolation parameters" )
    val stopBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    val stopNoBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    val stopBackoffDenom = new Log1dTable( Set[ObservedLabel]() )
    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1,h2) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffStopKey = StopOrNot(Word(h1), stopKey.dir, stopKey.adj)
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
                stopBackoffCounts( backoffStopKey, dec )
              )
            )
            stopBackoffDenom.setValue(
              stopKey.w,
              List(
                stopBackoffDenom( stopKey.w ),
                stopCounts( stopKey, dec ),
                stopBackoffCounts( backoffStopKey, dec )
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

    stopBackoffDenom.domain.foreach{ h =>
      newNoStopBackoffScore.setValue(
        h,
        expDigamma(
          Math.sumLogProb(
            stopNoBackoffSums( h ),
            math.log( noStopBackoff )
          )
        ) - expDigamma( stopBackoffDenom( h ) )
      )
      newStopBackoffScore.setValue(
        h,
        expDigamma(
          Math.sumLogProb(
            stopBackoffSums( h ),
            math.log( noStopBackoff )
          )
        ) - expDigamma( stopBackoffDenom( h ) )
      )
    }


    // whew, now let's get interpolation parameters for chooseScore

    // println( "Now, choose interpolation parameters" )
    val chooseBackoffDenom = new Log1dTable( Set[ObservedLabel]() )
    // println( "," )
    val chooseNoBackoffSums = new Log1dTable( Set[ObservedLabel]() )
    // println( ":" )
    val chooseBackoffArgSums = new Log1dTable( Set[ObservedLabel]() )
    // println( ";" )
    val chooseBackoffBothSums = new Log1dTable( Set[ObservedLabel]() )
    // println( "." )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseKey.h match {
        case WordPair( h1, h2 ) => {
          val backoffBothKey = ChooseArgument( Word( h1 ), chooseKey.dir )
          chooseCounts(chooseKey).keySet.par.foreach{ arg =>
            arg match {
              case WordPair( a1, a2 ) => {
                val sumsKey = new WordQuad( h1, h2, a1, a2 )
                val backoffArg = Word(a1)
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
                    chooseBackoffArgCounts( chooseKey, backoffArg )
                  )
                )
                chooseBackoffBothSums.setValue(
                  sumsKey,
                  Math.sumLogProb(
                    chooseBackoffBothSums( sumsKey ),
                    chooseBackoffBothCounts( backoffBothKey, backoffArg )
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
                        chooseBackoffArgCounts( chooseKey, backoffArg ),
                        chooseBackoffBothCounts( backoffBothKey, backoffArg )
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

    //println( "Ok, now the expdigammas are called" )

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
      newNoChooseBackoffScore.setValue(
        ha,
        expDigamma(
          Math.sumLogProb(
            chooseNoBackoffSums( ha ),
            math.log( noChooseBackoff )
          )
        ) - expDigammaDenom
      )
      newChooseBackoffArgScore.setValue(
        ha,
        expDigamma(
          Math.sumLogProb(
            chooseBackoffArgSums( ha ),
            math.log( backoffArg )
          )
        ) - expDigammaDenom
      )
      newChooseBackoffBothScore.setValue(
        ha,
        expDigamma(
          Math.sumLogProb(
            chooseBackoffBothSums( ha ),
            math.log( backoffBoth )
          )
        ) - expDigammaDenom
      )
    }

    //println( "Ok, now interpolate and project into the original grammar" )

    // Ok, now compute backed-off parameters
    val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    stopCounts.parents.foreach{ stopKey =>
      dmv.stopDecision.foreach{ stopDecision =>
        stopKey.w match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = StopOrNot( Word(h1), stopKey.dir, stopKey.adj )
            backedoffStop.setValue(
              stopKey,
              stopDecision,
              Math.sumLogProb(
                newNoStopBackoffScore(stopKey.w) + stopCounts( stopKey, stopDecision ),
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

    val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    chooseCounts.parents.foreach{ chooseKey =>
      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h1), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {
                val backoffKey = new WordQuad( h1, h2, a1, a2 )
                val backoffChosenArg = Word(a1)
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  List(
                    newNoChooseBackoffScore( backoffKey ) + chooseCounts( chooseKey, arg ),
                    newChooseBackoffArgScore( backoffKey ) +
                      chooseBackoffArgCounts( chooseKey, backoffChosenArg ),
                    newChooseBackoffBothScore( backoffKey ) +
                      chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg )
                  ).reduceLeft(Math.sumLogProb(_,_))
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
    backedoffChoose.expDigammaNormalize()

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

    // override def toDMVGrammar = {
    //   // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    //   val toReturn = associatedGrammar

    //   //print( "Computing DMVBayesianBackoffGrammar..." )

    //   //println( "chooseCounts before toDMVGrammar:\n" + chooseCounts )

        //   val stopBackoff = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
        //   stopCounts.parents.foreach{ stopKey =>
        //     dmv.stopDecision.foreach{ stopDecision =>
        //       stopKey.w match {
        //         case WordPair( h1, h2 ) => {
        //           val backoffHeadKey = StopOrNot( Word(h1), stopKey.dir, stopKey.adj )
        //           stopBackoff.setValue(
        //             backoffHeadKey,
        //             stopDecision,
        //             Math.sumLogProb(
        //               stopBackoff( backoffHeadKey, stopDecision ),
        //               stopCounts( stopKey, stopDecision )
        //             )
        //           )
        //         }
        //         case rootHead:AbstractRoot => {
        //         }
        //       }
        //     }
        //   }

        //   val chooseBackoffArg = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
        //   val chooseBackoffBoth = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
        //   chooseCounts.parents.foreach{ chooseKey =>
        //     //println( "Considering " + chooseKey )
        //     chooseCounts(chooseKey).keySet.foreach{ arg =>
        //       chooseKey.h match {
        //         case WordPair( h1, h2 ) => {
        //           val backoffHeadKey = ChooseArgument( Word(h1), chooseKey.dir )
        //           arg match {
        //             case WordPair( a1, a2 ) => {
        //               chooseBackoffArg.setValue(
        //                 chooseKey,
        //                 Word(a1),
        //                 Math.sumLogProb(
        //                   chooseBackoffArg( chooseKey, Word(a1) ),
        //                   chooseCounts( chooseKey, arg )
        //                 )
        //               )
        //               chooseBackoffBoth.setValue(
        //                 backoffHeadKey,
        //                 Word(a1),
        //                 Math.sumLogProb(
        //                   chooseBackoffBoth( backoffHeadKey, Word(a1) ),
        //                   chooseCounts( chooseKey, arg )
        //                 )
        //               )
        //             }
        //             case rootArg:AbstractRoot => {
        //             }
        //           }
        //         }
        //         case rootHead:AbstractRoot => {
        //         }
        //       }
        //     }
        //   }


    //   // println( "\n\nchooseCountsA:\n" + chooseCountsA )
    //   // println( "\n\nchooseCountsB:\n" + chooseCountsB + "\n\n" )

    //   stopCounts.expDigammaNormalize()
    //   stopBackoff.expDigammaNormalize()

    //   chooseCounts.expDigammaNormalize()
    //   chooseBackoffArg.expDigammaNormalize()
    //   chooseBackoffBoth.expDigammaNormalize()


    //   // Ok, now compute smoothed parameters
    //   val smoothedStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //   stopCounts.parents.foreach{ stopKey =>
    //     dmv.stopDecision.foreach{ stopDecision =>
    //       stopKey.w match {
    //         case WordPair( h1, h2 ) => {
    //           val backoffHeadKey = StopOrNot( Word(h1), stopKey.dir, stopKey.adj )
    //           smoothedStop.setValue(
    //             stopKey,
    //             stopDecision,
    //             Math.sumLogProb(
    //               stopCounts( stopKey, stopDecision ),
    //               stopBackoff( backoffHeadKey, stopDecision )
    //             )
    //           )
    //         }
    //         case rootHead:AbstractRoot => {
    //           smoothedStop.setValue(
    //             stopKey,
    //             stopDecision,
    //             stopCounts( stopKey, stopDecision )
    //           )
    //         }
    //       }
    //     }
    //   }

    //   val smoothedChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //   chooseCounts.parents.foreach{ chooseKey =>
    //     chooseCounts(chooseKey).keySet.foreach{ arg =>
    //       chooseKey.h match {
    //         case WordPair( h1, h2 ) => {
    //           val backoffHeadKey = ChooseArgument( Word(h1), chooseKey.dir )
    //           arg match {
    //             case WordPair( a1, a2 ) => {
    //               smoothedChoose.setValue(
    //                 chooseKey,
    //                 arg,
    //                 List(
    //                   chooseCounts( chooseKey, arg ),
    //                   chooseBackoffArg( chooseKey, Word(a1) ),
    //                   chooseBackoffBoth( backoffHeadKey, Word(a1) )
    //                 ).reduceLeft(Math.sumLogProb(_,_))
    //               )
    //             }
    //             case rootArg:AbstractRoot => {
    //               smoothedChoose.setValue(
    //                 chooseKey,
    //                 arg,
    //                 chooseCounts( chooseKey, arg )
    //               )
    //             }
    //           }
    //         }
    //         case rootHead:AbstractRoot => {
    //           smoothedChoose.setValue(
    //             chooseKey,
    //             arg,
    //             chooseCounts( chooseKey, arg )
    //           )
    //         }
    //       }
    //     }
    //   }


    //   toReturn.setParams(
    //     VanillaDMVParameters(
    //       orderCounts.toLogCPT,
    //       stopCounts.asLogCPT,
    //       chooseCounts.asLogCPT
    //       // stopCounts.toLogCPT,
    //       // chooseCounts.toLogCPT,
    //     )
    //   )

    //   //println( "Done!" )

    //   //toReturn.normalize
    //   toReturn
    // }

}

