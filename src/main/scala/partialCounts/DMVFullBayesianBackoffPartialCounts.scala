package predictabilityParsing.partialCounts

import scalala.library.Numerics.{lgamma,logSum}
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVFullBayesianBackoffGrammar
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
class DMVFullBayesianBackoffPartialCounts(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  // these are specific backoff parameters
  stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  headBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  argBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
) extends DMVPartialCounts {

  def this(
    noBackoffAlpha:Double,
    backoffAlpha:Double
  ) = this(
    noBackoffAlpha,
    backoffAlpha,
    // these are specific backoff parameters
    stopBackoffScore = Log2dTable(
      Set[StopOrNot](),
      dmv.backoffDecision,
      Map[BackoffDecision,Double](
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    ),
    headBackoffScore = Log2dTable(
      Set[ChooseArgument](),
      dmv.backoffDecision,
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
        }
      )
    ),
    argBackoffScore = Log2dTable(
      Set[ChooseArgument](),
      dmv.backoffDecision,
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
        }
      )
    )
  )
  def this() = this( 35, 70 ) // defaults inspired by Headden for use on wsj10

  // override def clearInterpolationScores {
  //   println( "clearing interpolation scores..." )
  //   val expDigammaDenom = Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
  //   val backoffDefault = Math.expDigamma( math.log( backoffAlpha ) ) - expDigammaDenom
  //   val noBackoffDefault = Math.expDigamma( math.log( noBackoffAlpha ) ) - expDigammaDenom
  //   stopBackoffScore.setCPT(
  //     Log2dTable(
  //       Set[StopOrNot](),
  //       dmv.backoffDecision,
  //       Map(
  //         Backoff -> backoffDefault,
  //         NotBackoff -> noBackoffAlpha
  //       )
  //     )
  //   )
  //   headBackoffScore.setCPT(
  //     Log2dTable(
  //       Set[ChooseArgument](),
  //       dmv.backoffDecision,
  //       Map(
  //         Backoff -> backoffDefault,
  //         NotBackoff -> noBackoffAlpha
  //       )
  //     )
  //   )
  // }

  override def associatedGrammar = new DMVFullBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    stopBackoffScore,
    headBackoffScore,
    argBackoffScore
  )

  def associatedGrammar(
    newStopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
    newHeadBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
    newArgBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
  ):AbstractDMVGrammar = new DMVFullBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    newStopBackoffScore,
    newHeadBackoffScore,
    newArgBackoffScore
  )

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    //val backoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val stopBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val headBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    val argBackoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )
    // val chooseBackoffAlphaMap = Map(
    //   BackoffArg -> backoffAlpha,
    //   BackoffHead -> backoffAlpha,
    //   BackoffBoth -> backoffAlpha,
    //   NotBackoff -> noBackoffAlpha
    // )


    // First, compute new interpolation parameters, starting with stop
    val stopBackoffInterpolationSums = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )
    //val stopBackoffInterpolationTiedSums = new Log1dTable( Set[StopOrNot]() )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    //val tiedStopRuleCount = collection.mutable.Map[StopOrNot,Int]().withDefaultValue( 0 )

    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1, h2 ) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffHead = Word(h2)
            val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

            /*
            val thisNoBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, NotBackoff )
            val thisBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, Backoff )
            */

            // assert( thisNoBackoffComponent >= Double.NegativeInfinity )
            // assert( thisBackoffComponent >= Double.NegativeInfinity )

            // for interpolation parameters, sum out backoff set
            stopBackoffInterpolationSums.setValue(
              stopKey,
              NotBackoff,
              logSum(
                stopBackoffInterpolationSums( stopKey, NotBackoff ),
                stopCounts( stopKey, dec )
              )
            )
            // stopBackoffInterpolationSums.setValue(
            //   stopKey,
            //   Backoff,
            //   logSum(
            //     stopBackoffInterpolationSums( stopKey, Backoff ),
            //     thisBackoffComponent
            //     //thisNoBackoffComponent
            //   )
            // )
            // Use backoff head because our interpolation rules are tied
            // stopBackoffInterpolationTiedSums.setValue(
            //   backoffStopKey,
            //   logSum(
            //     stopBackoffInterpolationTiedSums( backoffStopKey ),
            //     thisBackoffComponent
            //   )
            // )
            // tiedStopRuleCount( backoffStopKey ) += 1


            // for backoff distribution, only sum over elements of the backoff set.
            stopNoBackoffCounts.setValue(
              stopKey,
              dec,
              logSum(
                stopNoBackoffCounts( stopKey, dec ),
                //thisNoBackoffComponent
                stopCounts( stopKey, dec )
              )
            )
            stopBackoffCounts.setValue(
              backoffStopKey,
              dec,
              logSum(
                stopBackoffCounts( backoffStopKey, dec ),
                //thisBackoffComponent
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



    // whew, now let's get interpolation parameters for chooseScore

    val headBackoffInterpolationSums = new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )
    val argBackoffInterpolationSums = new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )


    // along with backoff terms
    val noChooseBackoffCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffArgBCounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )
    val backoffArgACounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )
    val backoffBothArgACounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )
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

                backoffArgACounts.setValue(
                  chooseKey,
                  argA,
                  logSum(
                    backoffArgACounts( chooseKey, argA ),
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

                backoffBothArgACounts.setValue(
                  backoffHeadKey,
                  argA,
                  logSum(
                    backoffBothArgACounts( backoffHeadKey, argA ),
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
    argBackoffInterpolationSums.expDigammaNormalize( argBackoffAlphaMap )

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
    backoffArgACounts.expDigammaNormalize()
    backoffArgBCounts.expDigammaNormalize()
    backoffBothArgACounts.expDigammaNormalize()
    backoffBothArgBCounts.expDigammaNormalize()
    rootChooseCounts.expDigammaNormalize()


    val chooseDefaults = collection.mutable.Map[ChooseArgument,Double]().withDefaultValue(
      expDigamma( 0 ) - expDigamma( math.log( chooseBackoffHeadCounts.parents.size ) )
    )

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

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          noChooseBackoffCounts.getParentDefault( chooseKey ),

                      headBackoffInterpolationSums( chooseKey, NotBackoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffArgACounts.getParentDefault( chooseKey ) +
                          backoffArgBCounts.getParentDefault( chooseKey ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          backoffHeadCounts.getParentDefault( backoffHeadKey ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffBothArgACounts.getParentDefault( chooseKey ) +
                          backoffBothArgBCounts.getParentDefault( chooseKey )

                                // chooseDefaults.getOrElse( chooseKey, Double.NegativeInfinity ),

                                // chooseBackoffInterpolationSums( chooseKey, NotBackoff ) +
                                //   noChooseBackoffCounts.getParentDefault( chooseKey ),

                                // chooseBackoffInterpolationSums( chooseKey, BackoffHead ) +
                                //   backoffHeadCounts.getParentDefault( backoffHeadKey ),

                                // chooseBackoffInterpolationSums( chooseKey, BackoffArg ) +
                                //   backoffArgACounts.getParentDefault( chooseKey ) +
                                //   backoffArgBCounts.getParentDefault( chooseKey ),

                                // chooseBackoffInterpolationSums( chooseKey, BackoffBoth ) +
                                //   backoffBothArgACounts.getParentDefault( backoffHeadKey ) +
                                //   backoffBothArgBCounts.getParentDefault( backoffHeadKey )
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
                          backoffArgACounts( chooseKey, argA ) +
                          backoffArgBCounts( chooseKey, argB ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, NotBackoff ) +
                          backoffHeadCounts( backoffHeadKey, argPair ),

                      headBackoffInterpolationSums( chooseKey, Backoff ) +
                        argBackoffInterpolationSums( argBackoffKey, Backoff ) +
                          backoffBothArgACounts( chooseKey, argA ) +
                          backoffBothArgBCounts( chooseKey, argB )

                                // chooseBackoffInterpolationSums( chooseKey, NotBackoff ) +
                                //   noChooseBackoffCounts( chooseKey, argPair ),
                                // chooseBackoffInterpolationSums( chooseKey, BackoffHead ) +
                                //   backoffHeadCounts( backoffHeadKey, argPair ),
                                // chooseBackoffInterpolationSums( chooseKey, BackoffArg ) +
                                //   backoffArgACounts( chooseKey, argA ) +
                                //   backoffArgBCounts( chooseKey, argB ),
                                // chooseBackoffInterpolationSums( chooseKey, BackoffBoth ) +
                                //   backoffBothArgACounts( backoffHeadKey, argA ) +
                                //   backoffBothArgBCounts( backoffHeadKey, argB )
                    )
                  )
                )
              }
              case rootArg:AbstractRoot => {
                /* intentionally empty */
                // backedoffChoose.setValue(
                //   chooseKey,
                //   arg,
                //   Double.NegativeInfinity
                // )
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
    backedoffChoose.setDefaultParentMap( chooseDefaults )

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
      DMVFullBayesianBackoffParameters(
        // freeEnergy,
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        stopBackoffInterpolationSums,
        headBackoffInterpolationSums,
        argBackoffInterpolationSums
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

