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
 */
class DMVBayesianBackoffPartialCounts(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  // these are specific backoff parameters
  stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  headBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
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
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( noBackoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    ),
    headBackoffScore = Log2dTable(
      Set[ChooseArgument](),
      dmv.backoffDecision,
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
  )
  def this() = this( 35, 70 ) // defaults inspired by Headden for use on wsj10

  override def clearInterpolationScores {
    println( "clearing interpolation scores..." )
    val expDigammaDenom = Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
    val backoffDefault = Math.expDigamma( math.log( backoffAlpha ) ) - expDigammaDenom
    val noBackoffDefault = Math.expDigamma( math.log( noBackoffAlpha ) ) - expDigammaDenom
    stopBackoffScore.setCPT(
      Log2dTable(
        Set[StopOrNot](),
        dmv.backoffDecision,
        Map(
          Backoff -> backoffDefault,
          NotBackoff -> noBackoffAlpha
        )
      )
    )
    headBackoffScore.setCPT(
      Log2dTable(
        Set[ChooseArgument](),
        dmv.backoffDecision,
        Map(
          Backoff -> backoffDefault,
          NotBackoff -> noBackoffAlpha
        )
      )
    )
  }

  override def associatedGrammar = new DMVBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    stopBackoffScore,
    headBackoffScore
  )

  def associatedGrammar(
    newStopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
    newBackoffHeadScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
  ):AbstractDMVGrammar = new DMVBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    newStopBackoffScore,
    newBackoffHeadScore
  )

  override def toDMVGrammar = {
    print( "Computing DMVBayesianBackoffGrammar..." )

    val backoffAlphaMap = Map( Backoff -> backoffAlpha, NotBackoff -> noBackoffAlpha )


    // First, compute new interpolation parameters, starting with stop
    val stopBackoffInterpolationSums = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )
    val stopBackoffInterpolationTiedSums = new Log1dTable( Set[StopOrNot]() )

    // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    val stopNoBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    val stopBackoffCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

    val tiedStopRuleCount = collection.mutable.Map[StopOrNot,Int]().withDefaultValue( 0 )


    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1, h2 ) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffHead = Word(h2)
            val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

            //assert( thisNoBackoffComponent >= Double.NegativeInfinity )
            //assert( thisBackoffComponent >= Double.NegativeInfinity )

            // for interpolation parameters, sum out backoff set
            stopBackoffInterpolationSums.setValue(
              stopKey,
              NotBackoff,
              logSum(
                //stopBackoffInterpolationSums( stopKey, NotBackoff ),
                //thisNoBackoffComponent
                stopBackoffInterpolationSums( stopKey, NotBackoff ),
                stopCounts( stopKey, dec )
              )
            )
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

            // val thisNoBackoffComponent =
            //   stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, NotBackoff )
            // val thisBackoffComponent =
            //   stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, Backoff )

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
      // val WordPair( h1, h2 ) = stopKey.w
      // val backoffHead = Word(h2)
      // val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

      stopBackoffInterpolationSums.setValue(
        stopKey,
        Backoff,
        Double.NegativeInfinity
        // stopBackoffInterpolationTiedSums( backoffStopKey ) -
        //   math.log( tiedStopRuleCount( backoffStopKey ) )
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

    val chooseBackoffHeadInterpolationSums =
      new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )


    // along with backoff terms
    val noBackoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

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

                // val thisNoBackoffHeadComponent =
                //   chooseCounts( chooseKey, arg ) + headBackoffScore( chooseKey, NotBackoff )
                // val thisBackoffHeadComponent =
                //   chooseCounts( chooseKey, arg ) + headBackoffScore( chooseKey, Backoff )

                noBackoffHeadCounts.setValue(
                  chooseKey,
                  backoffArg,
                  logSum(
                    noBackoffHeadCounts( chooseKey, backoffArg ),
                    //thisNoBackoffHeadComponent
                    chooseCounts( chooseKey, arg )
                  )
                )
                backoffHeadCounts.setValue(
                  backoffHeadKey,
                  backoffArg,
                  logSum(
                    backoffHeadCounts( backoffHeadKey, backoffArg ),
                    //thisBackoffHeadComponent
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



    chooseBackoffHeadInterpolationSums.parents.foreach{ chooseKey =>
      chooseBackoffHeadInterpolationSums.setValue(
        chooseKey,
        Backoff,
        Double.NegativeInfinity
      )
    }

    chooseBackoffHeadInterpolationSums.expDigammaNormalize( backoffAlphaMap )
    chooseBackoffHeadInterpolationSums.setDefaultChildMap(
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


    backoffHeadCounts.expDigammaNormalize()
    noBackoffHeadCounts.expDigammaNormalize()
    rootChooseCounts.expDigammaNormalize()

    val chooseDefaults = collection.mutable.Map[ChooseArgument,Double]().withDefaultValue(
      expDigamma( 0 ) - expDigamma( math.log( chooseBackoffHeadCounts.parents.size ) )
        // +1 for Root
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
                chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ) +
                  noBackoffHeadCounts.getParentDefault( chooseKey ),
                chooseBackoffHeadInterpolationSums( chooseKey, Backoff ) +
                  backoffHeadCounts.getParentDefault( backoffHeadKey )
              )
            )
        case rootHead:AbstractRoot => {
          // Special handling to allow only one root.
          chooseDefaults +=
            chooseKey -> rootChooseCounts.getParentDefault( chooseKey )
        }
      }

      chooseCounts(chooseKey).keySet.foreach{ arg =>
        chooseKey.h match {
          case WordPair( h1, h2 ) => {
            val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
            arg match {
              case WordPair( a1, a2 ) => {

                val backoffArg = Word(a2)

                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(
                      chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ) +
                        noBackoffHeadCounts( chooseKey, /*arg*/ backoffArg ),
                      chooseBackoffHeadInterpolationSums( chooseKey, Backoff ) +
                        backoffHeadCounts( backoffHeadKey, /*arg*/ backoffArg )
                    )
                  )
                )
              }
              case rootArg:AbstractRoot => {
                /* Intentionally empty */
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

    //println( "backed off choose parent default:\n" + chooseDefaults )

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

    // println( "Choose default: " +
    //   math.exp( backedoffChoose( ChooseArgument( Word( "wheigjuefd"), RightAttachment ) , Word(
    //   "3waf" ) ) )
    // )
    // println( "Stop default: " +
    //   math.exp( backedoffStop( StopOrNot( Word( "wheigjuefd"), RightAttachment,true ) , Stop ) )
    // )

    toReturn.setParams(
      DMVBayesianBackoffParameters(
        // freeEnergy,
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        stopBackoffInterpolationSums,
        chooseBackoffHeadInterpolationSums//,
        //chooseBackoffArgInterpolationSums
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

