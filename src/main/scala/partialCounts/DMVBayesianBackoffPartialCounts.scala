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
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  // these are specific backoff parameters
  stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  headBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  argBackoffScore:AbstractLog2dTable[WordPair,BackoffDecision]
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
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        }
      )
    ),
    argBackoffScore = Log2dTable(
      Set[WordPair](),
      dmv.backoffDecision,
      Map[BackoffDecision,Double](
        Backoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
            Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
        },
        NotBackoff -> {
          Math.expDigamma( math.log( backoffAlpha ) ) -
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
    argBackoffScore.setCPT(
      Log2dTable(
        Set[WordPair](),
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
    headBackoffScore,
    argBackoffScore
  )

  def associatedGrammar(
    newStopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
    newBackoffHeadScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
    newBackoffArgScore:AbstractLog2dTable[WordPair,BackoffDecision]
  ):AbstractDMVGrammar = new DMVBayesianBackoffGrammar(
    noBackoffAlpha,
    backoffAlpha,
    newStopBackoffScore,
    newBackoffHeadScore,
    newBackoffArgScore
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

    stopCounts.parents.foreach{ stopKey =>
      stopKey.w match {
        case WordPair( h1, h2 ) => {
          dmv.stopDecision.foreach{ dec =>
            val backoffHead = Word(h2)
            val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

            val thisNoBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, NotBackoff )
            val thisBackoffComponent =
              stopCounts( stopKey, dec ) + stopBackoffScore( stopKey, Backoff )

            // for interpolation parameters, sum out backoff set
            stopBackoffInterpolationSums.setValue(
              stopKey,
              NotBackoff,
              logSum(
                stopBackoffInterpolationSums( stopKey, NotBackoff ),
                thisNoBackoffComponent
              )
            )
            // Use backoff head because our interpolation rules are tied
            stopBackoffInterpolationTiedSums.setValue(
              backoffStopKey,
              logSum(
                stopBackoffInterpolationTiedSums( backoffStopKey ),
                thisBackoffComponent
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

    stopBackoffInterpolationSums.parents.foreach{ stopKey =>
      val WordPair( h1, h2 ) = stopKey.w
      val backoffHead = Word(h2)
      val backoffStopKey = StopOrNot( backoffHead, stopKey.dir, stopKey.adj )

      stopBackoffInterpolationSums.setValue(
        stopKey,
        Backoff,
        stopBackoffInterpolationTiedSums( stopKey )//, Backoff )
      )
    }

    val rootStopCounts =
      new Log2dTable( Set[StopOrNot](), dmv.stopDecision )

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
    val chooseBackoffHeadInterpolationTiedSums = new Log1dTable( Set[ChooseArgument]() )

    val chooseBackoffArgInterpolationSums =
      new Log2dTable( Set[Word](), dmv.backoffDecision )
    val chooseBackoffArgInterpolationTiedSums = new Log1dTable( Set[Word]() )

    // along with backoff terms
    val noBackoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    val backoffHeadCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

    val noBackoffArgCounts = new Log2dTable( Set[ChooseArgument](), Set[WordPair]() )
    val backoffArgCounts = new Log2dTable( Set[ChooseArgument](), Set[Word]() )

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

                val interpolationBackoffHead = Word( h2 )
                val interpolationBackoffHeadKey =
                  ChooseArgument( interpolationBackoffHead, attachDir )

                val argPair = WordPair( a1, a2 )
                val backoffArg = Word(a2)
                val recoveredLex = Word(a1)



                val thisNoBackoffHeadComponent =
                  chooseCounts( chooseKey, arg ) + headBackoffScore( chooseKey, NotBackoff )
                val thisBackoffHeadComponent =
                  chooseCounts( chooseKey, arg ) + headBackoffScore( chooseKey, Backoff )

                val thisNoBackoffArgComponent =
                  chooseCounts( chooseKey, arg ) + argBackoffScore( argPair, NotBackoff )
                val thisBackoffArgComponent =
                  chooseCounts( chooseKey, arg ) + argBackoffScore( argPair, Backoff )



                chooseBackoffHeadInterpolationSums.setValue(
                  chooseKey,
                  NotBackoff,
                  logSum(
                    chooseBackoffHeadInterpolationSums( chooseKey, NotBackoff ),
                    thisNoBackoffHeadComponent
                  )
                )
                chooseBackoffHeadInterpolationTiedSums.setValue(
                  interpolationBackoffHeadKey,
                  logSum(
                    chooseBackoffHeadInterpolationTiedSums( interpolationBackoffHeadKey ),
                    thisBackoffHeadComponent
                  )
                )

                chooseBackoffArgInterpolationSums.setValue(
                  argPair,
                  NotBackoff,
                  logSum(
                    chooseBackoffArgInterpolationSums( argPair, NotBackoff ),
                    thisNoBackoffArgComponent
                  )
                )
                chooseBackoffArgInterpolationTiedSums.setValue(
                  backoffArg,
                  logSum(
                    chooseBackoffArgInterpolationTiedSums( backoffArg ),
                    thisBackoffArgComponent
                  )
                )



                // Also, count up for each backoff distribution (not the interpolation parameters).
                noBackoffHeadCounts.setValue(
                  backoffHeadKey,
                  arg,
                  logSum(
                    backoffHeadCounts( backoffHeadKey, arg ),
                    thisBackoffHeadComponent
                  )
                )
                backoffHeadCounts.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    chooseNoBackoffCounts( chooseKey, arg ),
                    thisBackoffHeadComponent
                  )
                )
                noBackoffArgCounts.setValue(
                  backoffHeadKey,
                  argPair,
                  logSum(
                    noBackoffArgCounts( backoffHeadKey, argPair ),
                    thisNoBackoffArgComponent
                  )
                )
                backoffArgCounts.setValue(
                  backoffHeadKey,
                  backoffArg,
                  logSum(
                    backoffArgCounts( backoffHeadKey, backoffArg ),
                    thisBackoffArgComponent
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






          // //chooseBackoffInterpolationDenom.domain.foreach{ denom =>
          // chooseNoBackoffInterpolationSums.domain.foreach{ denom =>
          //   val WordQuad( h1, h2, a1, a2 ) = denom.h

          //   val interpolationBackoffHead = WordTriple( h2, a1, a2 )
          //   val interpolationBackoffArg = WordTriple( h1, h2, a2 )
          //   val interpolationBackoffBoth = WordPair( h2, a2 )

          //   val attachDir = denom.dir

          //   val interpolationBackoffArgKey =
          //     ChooseArgument( interpolationBackoffArg, attachDir )
          //   val interpolationBackoffHeadKey =
          //     ChooseArgument( interpolationBackoffHead, attachDir )
          //   val interpolationBackoffBothKey =
          //     ChooseArgument( interpolationBackoffBoth, attachDir )


          //   chooseBackoffInterpolationDenom.setValue(
          //     denom,
          //     logSum(
          //       Seq(
          //         //chooseBackoffInterpolationDenom( denom ),
          //         chooseNoBackoffInterpolationSums( denom ),
          //         chooseBackoffArgInterpolationSums( interpolationBackoffArgKey ),
          //         chooseBackoffHeadInterpolationSums( interpolationBackoffHeadKey ),
          //         chooseBackoffBothInterpolationSums( interpolationBackoffBothKey ),
          //         math.log( noChooseBackoff + backoffHead + backoffArg + backoffBoth )
          //       )
          //     )
          //   )
          // }

          // val newNoChooseBackoffScore = new Log1dTable( Set[ChooseArgument]() )
          // val newChooseBackoffHeadScore = new Log1dTable( Set[ChooseArgument]() )
          // val newChooseBackoffArgScore = new Log1dTable( Set[ChooseArgument]() )
          // val newChooseBackoffBothScore = new Log1dTable( Set[ChooseArgument]() )

          // val toCalculateFreeEnergyElementTwoBackoffArgNumerator = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffArgDenom = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffBothNumerator = Log1dTable( Set[ObservedLabel](), 0D )
          // val toCalculateFreeEnergyElementTwoBackoffBothDenom = Log1dTable( Set[ObservedLabel]() , 0D)

          // //println( "Estimating new choose interpolation distributions." )
          // chooseBackoffInterpolationDenom.domain.foreach{ ha =>
          //   val WordQuad( h1, h2, a1, a2 ) = ha.h

          //   val interpolationBackoffHead = WordTriple( h2,a1,a2)
          //   val interpolationBackoffArg = WordTriple( h1,h2,a2)
          //   val interpolationBackoffBoth = WordPair( h2, a2 )

          //   val attachDir = ha.dir

          //   val backoffArgKey =
          //     ChooseArgument( interpolationBackoffArg, attachDir )
          //   val backoffHeadKey =
          //     ChooseArgument( interpolationBackoffHead, attachDir )
          //   val backoffBothKey =
          //     ChooseArgument( interpolationBackoffBoth, attachDir )

          //   val expDigammaDenom = Math.expDigamma( chooseBackoffInterpolationDenom( ha ) )



          //   val thisChooseNoBackoffTotal = logSum(
          //     chooseNoBackoffInterpolationSums( ha ),
          //     math.log( noChooseBackoff )
          //   )
          //   newNoChooseBackoffScore.setValue(
          //     ha,
          //     expDigamma(
          //       thisChooseNoBackoffTotal
          //     ) - expDigammaDenom
          //   )


          //   val thisChooseBackoffHeadTotal = logSum(
          //     chooseBackoffHeadInterpolationSums( backoffHeadKey ),
          //     math.log( backoffHead )
          //   )
          //   newChooseBackoffHeadScore.setValue(
          //     ha,
          //     expDigamma(
          //       thisChooseBackoffHeadTotal
          //     ) - expDigammaDenom
          //   )


          //   val thisChooseBackoffArgTotal = logSum(
          //     chooseBackoffArgInterpolationSums( backoffArgKey ),
          //     math.log( backoffArg )
          //   )
          //   newChooseBackoffArgScore.setValue(
          //     ha,
          //     expDigamma(
          //       thisChooseBackoffArgTotal
          //     ) - expDigammaDenom
          //   )


          //   val thisChooseBackoffBothTotal = logSum(
          //     chooseBackoffBothInterpolationSums( backoffBothKey ),
          //     math.log( backoffBoth )
          //   )
          //   newChooseBackoffBothScore.setValue(
          //     ha,
          //     expDigamma(
          //       thisChooseBackoffBothTotal
          //     ) - expDigammaDenom
          //   )

          //         // val realSpaceNoBackoffTotal = math.exp( thisChooseNoBackoffTotal )
          //         // val realSpaceBackoffArgTotal = math.exp( thisChooseBackoffArgTotal )
          //         // val realSpaceBackoffBothTotal = math.exp( thisChooseBackoffBothTotal )

          //         // freeEnergyElementTwo += lgamma( realSpaceNoBackoffTotal ) - lgamma( noChooseBackoff )

          //         // toCalculateFreeEnergyElementTwoBackoffArgNumerator.setValue(
          //         //   backoffArgKey,
          //         //   toCalculateFreeEnergyElementTwoBackoffArgNumerator( backoffArgKey ) +
          //         //     realSpaceBackoffArgTotal
          //         // )
          //         // toCalculateFreeEnergyElementTwoBackoffArgDenom.setValue(
          //         //   backoffArgKey,
          //         //   toCalculateFreeEnergyElementTwoBackoffArgDenom( backoffArgKey ) +
          //         //     backoffArg
          //         // )

          //         // toCalculateFreeEnergyElementTwoBackoffBothNumerator.setValue(
          //         //   backoffBothKey,
          //         //   toCalculateFreeEnergyElementTwoBackoffBothNumerator( backoffBothKey ) +
          //         //     realSpaceBackoffBothTotal
          //         // )
          //         // toCalculateFreeEnergyElementTwoBackoffBothDenom.setValue(
          //         //   backoffBothKey,
          //         //   toCalculateFreeEnergyElementTwoBackoffBothDenom( backoffBothKey ) +
          //         //     backoffBoth
          //         // )

          //         // freeEnergyElementThree += lgamma( realSpaceNoBackoffTotal ) - lgamma( noStopBackoff )
          //         // freeEnergyElementThree += lgamma( realSpaceBackoffArgTotal ) - lgamma( backoffArg )
          //         // freeEnergyElementThree += lgamma( realSpaceBackoffBothTotal ) - lgamma( backoffBoth )

          //         // freeEnergyElementFour +=
          //         //   ( realSpaceNoBackoffTotal - noChooseBackoff ) * newNoChooseBackoffScore( ha )
          //         // freeEnergyElementFour +=
          //         //   ( realSpaceBackoffArgTotal - backoffArg ) * newChooseBackoffArgScore( ha )
          //         // freeEnergyElementFour +=
          //         //   ( realSpaceBackoffBothTotal - backoffBoth ) * newChooseBackoffBothScore( ha )
          // }

          // val chooseBackoffDefaultDenom = expDigamma(
          //   math.log( noChooseBackoff + backoffHead + backoffArg + backoffBoth )
          // )

          // newNoChooseBackoffScore.setDefault(
          //   expDigamma( math.log( noChooseBackoff ) ) -
          //     chooseBackoffDefaultDenom
          // )
          // newChooseBackoffHeadScore.setDefault(
          //   expDigamma( math.log( backoffHead ) ) -
          //     chooseBackoffDefaultDenom
          // )
          // newChooseBackoffArgScore.setDefault(
          //   expDigamma( math.log( backoffArg ) ) -
          //     chooseBackoffDefaultDenom
          // )
          // newChooseBackoffBothScore.setDefault(
          //   expDigamma( math.log( backoffBoth ) ) -
          //     chooseBackoffDefaultDenom
          // )



    // Ok, now compute backed-off parameters
    //println( "Estimating new stop distributions with backoff." )

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
                //     newChooseBackoffArgScore( backoffKey ) + chooseRecoverDepLexSums( arg, ,
                //     newChooseBackoffBothScore( backoffKey )
                //   ) <= 0D
                // )
                // assert( newNoChooseBackoffScore( backoffKey ) <= 0D )
                // assert( newChooseBackoffArgScore( backoffKey ) <= 0D )
                // assert( newChooseBackoffBothScore( backoffKey ) <= 0D )
                val recoveredLex = Word( a1 )
                chooseDefaults +=
                  chooseKey ->
                    logSum(
                      Seq(
                        //chooseDefaults( chooseKey ),
                        newChooseBackoffArgScore.getDefault +
                          chooseBackoffArgCounts.getDefault( chooseKey ) +
                            chooseRecoverDepLexSums( backoffChosenArg, recoveredLex ),
                        newChooseBackoffBothScore.getDefault +
                          chooseBackoffBothCounts.getDefault( backoffHeadKey ) +
                            chooseRecoverDepLexSums( backoffChosenArg, recoveredLex )
                      )
                    )
                println( chooseKey + " --> " + arg + "\n\t" + " ( " +
                  (backoffKey, backoffHeadKey, backoffChosenArg, recoveredLex ) +
                      math.exp( newChooseBackoffArgScore( backoffKey ) ) + " * " +
                        math.exp( chooseBackoffArgCounts( chooseKey, backoffChosenArg ) ) + " * " +
                          math.exp( chooseRecoverDepLexSums( backoffChosenArg, recoveredLex ) ) +
                            " = " +
                      math.exp(
                        newChooseBackoffArgScore( backoffKey ) +
                          chooseBackoffArgCounts( chooseKey, backoffChosenArg ) +
                            chooseRecoverDepLexSums( backoffChosenArg, recoveredLex )
                      )
                )
                backedoffChoose.setValue(
                  chooseKey,
                  arg,
                  logSum(
                    Seq(
                      //backedoffChoose( chooseKey, arg ),
                      newNoChooseBackoffScore( backoffKey ) + chooseNoBackoffCounts( chooseKey, arg ),
                      newChooseBackoffHeadScore( backoffKey ) +
                        chooseBackoffHeadCounts( backoffHeadKey, arg ),
                      newChooseBackoffArgScore( backoffKey ) +
                        chooseBackoffArgCounts( chooseKey, backoffChosenArg ) +
                          chooseRecoverDepLexSums( backoffChosenArg, recoveredLex ),
                      newChooseBackoffBothScore( backoffKey ) +
                        chooseBackoffBothCounts( backoffHeadKey, backoffChosenArg ) +
                          chooseRecoverDepLexSums( backoffChosenArg, recoveredLex )
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

    println( "newNoStopBackoffScore:\n" +newNoStopBackoffScore )

    toReturn.setParams(
      DMVBayesianBackoffParameters(
        // freeEnergy,
        orderCounts.toLogCPT,
        backedoffStop.asLogCPT,
        backedoffChoose.asLogCPT,
        stopBackoffInterpolationSums,
        chooseBackoffHeadInterpolationSums,
        chooseBackoffArgInterpolationSums
      )
    )

    toReturn

  }

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffAlpha: " + backoffAlpha + "\n" +


}

