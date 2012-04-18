package predictabilityParsing.grammars

import collection.mutable.Map

import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffJointDepsPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffJointDepsGrammar(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70
) extends DMVGrammar {

  val stopBackoffInterpolationScore = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )

  val stopNoBackoffScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val stopBackoffScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )


  val chooseBackoffHeadInterpolationScore =
    new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

  val noBackoffHeadScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val backoffHeadScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val rootChooseScore =
    new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )


  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =// p_order( word, pref )
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }

  override def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) = {// p_stop( stopKey, stopDecision )
    stopKey.w match {
      case rootHead:AbstractRoot => {
        if( stopKey.dir == RightAttachment )
          if( stopDecision == Stop ) 0 else Double.NegativeInfinity
        else if( stopKey.adj && ( stopDecision == Stop ) ) // Should be able to xor this...
          Double.NegativeInfinity
        else if( stopKey.adj && ( stopDecision == NotStop ) )
          0D
        else if( (!stopKey.adj) && ( stopDecision == Stop ) )
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
  }
  override def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) = {// p_stop( stopKey, stopDecision )
    chooseKey.h match {
      case rootHead:AbstractRoot => {
        rootChooseScore( chooseKey, arg )
      }
      case WordPair( _, h2 ) => {
        arg match {
          case rootArg:AbstractRoot => Double.NegativeInfinity
          case obsArg:WordPair => {
            logSum(
              Seq(
                chooseBackoffHeadInterpolationScore( chooseKey, NotBackoff ) +
                  noBackoffHeadScore( chooseKey, arg ),
                chooseBackoffHeadInterpolationScore( chooseKey, Backoff ) +
                  backoffHeadScore( ChooseArgument( Word(h2), chooseKey.dir ), arg )
              )
            )
          }
        }
      }
    }
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

  override def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVBayesianBackoffParameters(
      newStopBackoffInterpolationScore,
      newStopNoBackoffScore,
      newStopBackoffScore,
      newChooseBackoffHeadInterpolationScore,
      newNoBackoffHeadScore,
      newBackoffHeadScore,
      newRootChooseScore
    ) = parameters

    stopBackoffInterpolationScore.setCPT( newStopBackoffInterpolationScore )
    stopNoBackoffScore.setCPT( newStopNoBackoffScore )
    stopBackoffScore.setCPT( newStopBackoffScore )

    chooseBackoffHeadInterpolationScore.setCPT( newChooseBackoffHeadInterpolationScore )
    noBackoffHeadScore.setCPT( newNoBackoffHeadScore )
    backoffHeadScore.setCPT( newBackoffHeadScore )

    rootChooseScore.setCPT( newRootChooseScore )

    // p_order.setCPT( otherP_order )
    // p_stop.setCPT( otherP_stop )
    // p_choose.setCPT( otherP_choose )
    // stopBackoffScore.setCPT( otherStopBackoffScore )
    // headBackoffScore.setCPT( otherBackoffHeadScore )

    // p_stop.setValue(
    //   StopOrNot( Root, RightAttachment, true ),
    //   Stop,
    //   0D
    // )
    // p_stop.setValue(
    //   StopOrNot( Root, RightAttachment, true ),
    //   NotStop,
    //   Double.NegativeInfinity
    // )
    // p_stop.setValue(
    //   StopOrNot( Root, RightAttachment, false ),
    //   Stop,
    //   0D
    // )
    // p_stop.setValue(
    //   StopOrNot( Root, RightAttachment, false ),
    //   NotStop,
    //   Double.NegativeInfinity
    // )

    // p_stop.setValue(
    //   StopOrNot( Root, LeftAttachment, true ),
    //   NotStop,
    //   0D
    // )
    // p_stop.setValue(
    //   StopOrNot( Root, LeftAttachment, true ),
    //   Stop,
    //   Double.NegativeInfinity
    // )

    // p_stop.setValue(
    //   StopOrNot( Root, LeftAttachment, false ),
    //   NotStop,
    //   Double.NegativeInfinity
    // )
    // p_stop.setValue(
    //   StopOrNot( Root, LeftAttachment, false ),
    //   Stop,
    //   0D
    // )

  }

  // override def getParams = {
  //   VanillaDMVParameters(
  //     p_order,
  //     p_stop,
  //     p_choose
  //   )
  // }

  override def getParams = {
    DMVBayesianBackoffParameters(
      stopBackoffInterpolationScore,
      stopNoBackoffScore,
      stopBackoffScore,
      chooseBackoffHeadInterpolationScore,
      noBackoffHeadScore,
      backoffHeadScore,
      rootChooseScore
    )
  }


  override def emptyPartialCounts = new DMVBayesianBackoffJointDepsPartialCounts(
    noBackoffAlpha,
    backoffAlpha
  )

  override def toString =
    "DMVBayesianBackoffJointDepsGrammar\n" +
    "Alphas:\n" +
    "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
    "\tbackoffAlpha: " + backoffAlpha + "\n" +
    "stopBackoffInterpolationScore:\n" + stopBackoffInterpolationScore + "\n" +
    "stopNoBackoffScore:\n" + stopNoBackoffScore + "\n" +
    "stopBackoffScore:\n" + stopBackoffScore + "\n" +
    "chooseBackoffHeadInterpolationScore:\n" + chooseBackoffHeadInterpolationScore + "\n" +
    "noBackoffHeadScore:\n" + noBackoffHeadScore + "\n" +
    "backoffHeadScore:\n" + backoffHeadScore + "\n"
}

