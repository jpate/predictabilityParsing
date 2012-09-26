package predictabilityParsing.grammars

import collection.mutable.Map

import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffIndependentDepsPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffIndependentDepsGrammar(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  dmvRulesAlpha:Double = 1
) extends DMVGrammar {

  val stopBackoffInterpolationScore = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )

  val stopNoBackoffScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val stopBackoffScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )


  val chooseBackoffHeadInterpolationScore =
    new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

  val noBackoffHeadAScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val noBackoffHeadBScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val backoffHeadAScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val backoffHeadBScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  val rootChooseAScore =
    new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val rootChooseBScore =
    new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )



  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =// p_order( word, pref )
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }


      // override def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) = {// p_stop( stopKey, stopDecision )
      //   stopKey.w match {
      //     case rootHead:AbstractRoot => {
      //       if( stopKey.dir == RightAttachment )
      //         if( stopDecision == Stop ) 0 else Double.NegativeInfinity
      //       else if( stopKey.adj && ( stopDecision == Stop ) ) // Should be able to xor this...
      //         Double.NegativeInfinity
      //       else if( stopKey.adj && ( stopDecision == NotStop ) )
      //         0D
      //       else if( !(stopKey.adj) && ( stopDecision == Stop ) )
      //         0D
      //       else
      //         Double.NegativeInfinity
      //     }
      //     case WordPair( _, h2 ) => {
      //       logSum(
      //         Seq(
      //           stopBackoffInterpolationScore( stopKey, NotBackoff ) + stopNoBackoffScore( stopKey, stopDecision ),
      //           stopBackoffInterpolationScore( stopKey, Backoff ) +
      //             stopBackoffScore( StopOrNot( Word( h2 ), stopKey.dir, stopKey.adj ), stopDecision )
      //         )
      //       )
      //     }
      //   }
      // }
      // override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) = {// p_stop( stopKey, stopDecision )
      //   chooseKey.h match {
      //     case rootHead:AbstractRoot => {
      //       rootChooseScore( chooseKey, arg )
      //     }
      //     case WordPair( _, h2 ) => {
      //       arg match {
      //         case rootArg:AbstractRoot => Double.NegativeInfinity
      //         case WordPair( d1, d2 ) => {
      //           val argA = Word( d1 )
      //           val argB = Word( d2 )
      //           logSum(
      //             Seq(
      //               chooseBackoffHeadInterpolationScore( chooseKey, NotBackoff ) +
      //                 noBackoffHeadAScore( chooseKey, argA ) +
      //                 noBackoffHeadBScore( chooseKey, argB ),
      //               chooseBackoffHeadInterpolationScore( chooseKey, Backoff ) +
      //                 backoffHeadAScore( ChooseArgument( Word(h2), chooseKey.dir ), argA ) +
      //                 backoffHeadBScore( ChooseArgument( Word(h2), chooseKey.dir ), argB )
      //             )
      //           )
      //         }
      //       }
      //     }
      //   }
      // }


  ///////

  protected def stop_aux( stopKey:StopOrNot, stopDecision:StopDecision ) = {
    val thisScore = 
      stopKey.w match {
        case rootHead:AbstractRoot => {
          if( stopKey.dir == RightAttachment )
            if( stopDecision == Stop ) 0 else Double.NegativeInfinity
          else if( stopKey.adj && ( stopDecision == Stop ) ) // Should be able to xor this...
            Double.NegativeInfinity
          else if( stopKey.adj && ( stopDecision == NotStop ) )
            0D
          else if( !(stopKey.adj) && ( stopDecision == Stop ) )
            0D
          else
            Double.NegativeInfinity
        }
        case WordPair( _, h2 ) => {
          logSum(
            Seq(
              stopBackoffInterpolationScore( stopKey, NotBackoff ) + stopNoBackoffScore( stopKey, stopDecision ),
              stopBackoffInterpolationScore( stopKey, Backoff ) +
                stopBackoffScore( StopOrNot( Word( h2 ), stopKey.dir, stopKey.adj ), stopDecision )
            )
          )
        }
      }
    //p_stop.setValue( stopKey, stopDecision, thisScore )
    thisScore
  }

  override def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) =
    if( p_stop.definedAt( stopKey, stopDecision ) )
      p_stop( stopKey, stopDecision )
    else
      stop_aux( stopKey, stopDecision )

  protected def choose_aux( chooseKey:ChooseArgument, arg:ObservedLabel ) = {
    val thisScore =
      chooseKey.h match {
        case rootHead:AbstractRoot => {
          arg match {
            case rootArg:AbstractRoot => Double.NegativeInfinity
            case WordPair( d1, d2 ) => {
              rootChooseAScore( chooseKey, Word(d1) ) +
              rootChooseBScore( chooseKey, Word(d2) )
            }
          }
        }
        case WordPair( _, h2 ) => {
          arg match {
            case rootArg:AbstractRoot => Double.NegativeInfinity
            case WordPair( d1, d2 ) => {
              val argA = Word( d1 )
              val argB = Word( d2 )
              val backoffHeadKey = ChooseArgument( Word(h2), chooseKey.dir )
              logSum(
                Seq(
                  chooseBackoffHeadInterpolationScore( chooseKey, NotBackoff ) +
                    noBackoffHeadAScore( chooseKey, argA ) +
                    noBackoffHeadBScore( chooseKey, argB ),
                  chooseBackoffHeadInterpolationScore( chooseKey, Backoff ) +
                    backoffHeadAScore( backoffHeadKey, argA ) +
                    backoffHeadBScore( backoffHeadKey, argB ) 
                )
              )
            }
          }
        }
      }
    //p_choose.setValue( chooseKey, arg, thisScore )
    thisScore
  }

  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) =
    if( p_choose.definedAt( chooseKey, arg ) )
      p_choose( chooseKey, arg )
    else
      choose_aux( chooseKey, arg )






  override def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVBayesianBackoffIndependentDepsParameters(
      newBackedoffStop,
      newBackedoffChoose,
      newStopBackoffInterpolationScore,
      newStopNoBackoffScore,
      newStopBackoffScore,
      newChooseBackoffHeadInterpolationScore,
      newNoBackoffHeadAScore,
      newNoBackoffHeadBScore,
      newBackoffHeadAScore,
      newBackoffHeadBScore,
      newRootChooseAScore,
      newRootChooseBScore
    ) = parameters

    p_stop.setCPT( newBackedoffStop )
    p_choose.setCPT( newBackedoffChoose )

    stopBackoffInterpolationScore.setCPT( newStopBackoffInterpolationScore )
    stopNoBackoffScore.setCPT( newStopNoBackoffScore )
    stopBackoffScore.setCPT( newStopBackoffScore )


    chooseBackoffHeadInterpolationScore.setCPT( newChooseBackoffHeadInterpolationScore )

    noBackoffHeadAScore.setCPT( newNoBackoffHeadAScore )
    noBackoffHeadBScore.setCPT( newNoBackoffHeadBScore )

    backoffHeadAScore.setCPT( newBackoffHeadAScore )
    backoffHeadBScore.setCPT( newBackoffHeadBScore )

    rootChooseAScore.setCPT( newRootChooseAScore )
    rootChooseBScore.setCPT( newRootChooseBScore )


    // p_order.setCPT( otherP_order )
    // p_stop.setCPT( otherP_stop )
    // p_choose.setCPT( otherP_choose )
    // stopBackoffScore.setCPT( otherStopBackoffScore )
    // headBackoffScore.setCPT( otherBackoffHeadScore )

    p_stop.setValue(
      StopOrNot( Root, RightAttachment, true ),
      Stop,
      0D
    )
    p_stop.setValue(
      StopOrNot( Root, RightAttachment, true ),
      NotStop,
      Double.NegativeInfinity
    )
    p_stop.setValue(
      StopOrNot( Root, RightAttachment, false ),
      Stop,
      0D
    )
    p_stop.setValue(
      StopOrNot( Root, RightAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )

    p_stop.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      NotStop,
      0D
    )
    p_stop.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      Stop,
      Double.NegativeInfinity
    )

    p_stop.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )
    p_stop.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      Stop,
      0D
    )

  }

      // override def setParams[P<:DMVParameters]( parameters:P ) {
      //   val VanillaDMVParameters(
      //     otherP_order,
      //     otherP_stop,
      //     otherP_choose
      //   ) = parameters

      //   p_order.setCPT( otherP_order )
      //   p_stop.setCPT( otherP_stop )
      //   p_choose.setCPT( otherP_choose )

      //   p_stop.setValue(
      //     StopOrNot( Root, RightAttachment, true ),
      //     Stop,
      //     0D
      //   )
      //   p_stop.setValue(
      //     StopOrNot( Root, RightAttachment, true ),
      //     NotStop,
      //     Double.NegativeInfinity
      //   )
      //   p_stop.setValue(
      //     StopOrNot( Root, RightAttachment, false ),
      //     Stop,
      //     0D
      //   )
      //   p_stop.setValue(
      //     StopOrNot( Root, RightAttachment, false ),
      //     NotStop,
      //     Double.NegativeInfinity
      //   )

      //   p_stop.setValue(
      //     StopOrNot( Root, LeftAttachment, true ),
      //     NotStop,
      //     0D
      //   )
      //   p_stop.setValue(
      //     StopOrNot( Root, LeftAttachment, true ),
      //     Stop,
      //     Double.NegativeInfinity
      //   )

      //   p_stop.setValue(
      //     StopOrNot( Root, LeftAttachment, false ),
      //     NotStop,
      //     Double.NegativeInfinity
      //   )
      //   p_stop.setValue(
      //     StopOrNot( Root, LeftAttachment, false ),
      //     Stop,
      //     0D
      //   )

      // }

      // override def getParams = {
      //   VanillaDMVParameters(
      //     p_order,
      //     p_stop,
      //     p_choose
      //   )
      // }
  override def getParams = {
    DMVBayesianBackoffIndependentDepsParameters(
      p_stop,
      p_choose,
      stopBackoffInterpolationScore,
      stopNoBackoffScore,
      stopBackoffScore,
      chooseBackoffHeadInterpolationScore,
      noBackoffHeadAScore,
      noBackoffHeadBScore,
      backoffHeadAScore,
      backoffHeadBScore,
      rootChooseAScore,
      rootChooseBScore
    )
  }


  override def emptyPartialCounts = new DMVBayesianBackoffIndependentDepsPartialCounts(
    noBackoffAlpha,
    backoffAlpha,
    dmvRulesAlpha
  )

  // override def toString =
  //   //super.toString +
  //   "DMVBayesianBackoffIndependentDepsGrammar\n" +
  //   "Alphas:\n" +
  //   "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
  //   "\tbackoffAlpha: " + backoffAlpha + "\n" +
  //   "stopBackoffInterpolationScore:\n" + stopBackoffInterpolationScore + "\n" +
  //   "stopNoBackoffScore:\n" + stopNoBackoffScore + "\n" +
  //   "stopBackoffScore:\n" + stopBackoffScore + "\n" +
  //   "chooseBackoffHeadInterpolationScore:\n" + chooseBackoffHeadInterpolationScore + "\n" +
  //   "noBackoffHeadAScore:\n" + noBackoffHeadAScore + "\n" +
  //   "noBackoffHeadBScore:\n" + noBackoffHeadBScore + "\n" +
  //   "backoffHeadAScore:\n" + backoffHeadAScore + "\n" +
  //   "backoffHeadBScore:\n" + backoffHeadBScore + "\n"

  override def toString =
    "DMVBayesianBackoffGrammar\n" + 
    "Alphas:\n" +
    "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
    "\tbackoffAlpha: " + backoffAlpha + "\n" +
    "\tdmvRulesAlpha: " + dmvRulesAlpha + "\n" +
    "P_Stop (" + math.exp( p_stop.getDefault ) + "):\n" + p_stop +
    "P_Choose (" + math.exp( p_choose.getDefault ) + "):\n" + p_choose +
    "stopBackoffInterpolationScore:\n" + stopBackoffInterpolationScore + "\n" +
    "stopNoBackoffScore:\n" + stopNoBackoffScore + "\n" +
    "stopBackoffScore:\n" + stopBackoffScore + "\n" +
    "chooseBackoffHeadInterpolationScore:\n" + chooseBackoffHeadInterpolationScore + "\n" +
    "noBackoffHeadAScore:\n" + noBackoffHeadAScore + "\n" +
    "noBackoffHeadBScore:\n" + noBackoffHeadBScore + "\n" +
    "backoffHeadAScore:\n" + backoffHeadAScore + "\n" +
    "backoffHeadBScore:\n" + backoffHeadBScore + "\n"
}

