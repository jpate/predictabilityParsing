package predictabilityParsing.grammars

import collection.mutable.Map

import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffThreeStreamPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffThreeStreamGrammar(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffOneAlpha:Double = 52.5,
  backoffTwoAlpha:Double = 70
) extends DMVGrammar {


  val stopBackoffFirstInterpolationScore = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )
  val stopBackoffSecondInterpolationScore = new Log2dTable( Set[StopOrNot](), dmv.backoffDecision )

  val stopNoBackoffScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val stopBackoffOnceScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val stopBackoffTwiceScore = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )


  val chooseBackoffHeadFirstInterpolationScore =
    new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )
  val chooseBackoffHeadSecondInterpolationScore =
    new Log2dTable( Set[ChooseArgument](), dmv.backoffDecision )

  val noBackoffHeadScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val backoffHeadOnceScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
  val backoffHeadTwiceScore = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  val rootChooseScore =
    new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

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
        case WordTriple( _, h2, h3 ) => {

          val backoffHeadOnce = WordPair(h2, h3)
          val backoffStopOnceKey = StopOrNot( backoffHeadOnce, stopKey.dir, stopKey.adj )

          val backoffHeadTwice = Word(h3)
          val backoffStopTwiceKey = StopOrNot( backoffHeadTwice, stopKey.dir, stopKey.adj )

          logSum(
            Seq(
              stopBackoffFirstInterpolationScore( stopKey, NotBackoff ) +
                stopNoBackoffScore( stopKey, stopDecision ),

              stopBackoffFirstInterpolationScore( stopKey, Backoff ) +
                stopBackoffSecondInterpolationScore( backoffStopOnceKey, NotBackoff ) +
                  stopBackoffOnceScore( backoffStopOnceKey, stopDecision ),

              stopBackoffFirstInterpolationScore( stopKey, Backoff ) +
                stopBackoffSecondInterpolationScore( backoffStopOnceKey, Backoff ) +
                  stopBackoffTwiceScore( backoffStopTwiceKey, stopDecision )
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
          rootChooseScore( chooseKey, arg )
        }
        case WordTriple( _, h2, h3 ) => {
          arg match {
            case rootArg:AbstractRoot => Double.NegativeInfinity
            case WordTriple( _, _, d3 ) => {
              val backoffHeadOnce = WordPair(h2, h3)
              val backoffHeadOnceKey = ChooseArgument( backoffHeadOnce, chooseKey.dir )

              val backoffHeadTwice = Word(h3)
              val backoffHeadTwiceKey = ChooseArgument( backoffHeadTwice, chooseKey.dir )

              val backoffArg = Word( d3 )

              logSum(
                Seq(
                  chooseBackoffHeadFirstInterpolationScore( chooseKey, NotBackoff ) +
                    noBackoffHeadScore( chooseKey, backoffArg ),

                  chooseBackoffHeadFirstInterpolationScore( chooseKey, Backoff ) +
                    chooseBackoffHeadSecondInterpolationScore( backoffHeadOnceKey, NotBackoff ) +
                      backoffHeadOnceScore( backoffHeadOnceKey, backoffArg ),

                  chooseBackoffHeadFirstInterpolationScore( chooseKey, Backoff ) +
                    chooseBackoffHeadSecondInterpolationScore( backoffHeadOnceKey, Backoff ) +
                      backoffHeadTwiceScore( backoffHeadTwiceKey, backoffArg )
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



  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }


  override def setParams[P<:DMVParameters]( parameters:P ) {
    // val VanillaDMVParameters(
    //   otherP_order,
    //   otherP_stop,
    //   otherP_choose
    // ) = parameters

    val DMVBayesianBackoffThreeStreamParameters(
      newBackedoffStop,
      newBackedoffChoose,
      newStopBackoffFirstInterpolationScore,
      newStopBackoffSecondInterpolationScore,
      newStopNoBackoffScore,
      newStopBackoffOnceScore,
      newStopBackoffTwiceScore,
      newChooseBackoffHeadFirstInterpolationScore,
      newChooseBackoffHeadSecondInterpolationScore,
      newNoBackoffHeadScore,
      newBackoffHeadOnceScore,
      newBackoffHeadTwiceScore,
      newRootChooseScore
    ) = parameters

    //p_order.setCPT( otherP_order )
    p_stop.setCPT( newBackedoffStop )
    p_choose.setCPT( newBackedoffChoose )

    stopBackoffFirstInterpolationScore.setCPT( newStopBackoffFirstInterpolationScore )
    stopBackoffSecondInterpolationScore.setCPT( newStopBackoffSecondInterpolationScore )
    stopNoBackoffScore.setCPT( newStopNoBackoffScore )
    stopBackoffOnceScore.setCPT( newStopBackoffOnceScore )
    stopBackoffTwiceScore.setCPT( newStopBackoffTwiceScore )

    chooseBackoffHeadFirstInterpolationScore.setCPT( newChooseBackoffHeadFirstInterpolationScore )
    chooseBackoffHeadSecondInterpolationScore.setCPT( newChooseBackoffHeadSecondInterpolationScore )
    noBackoffHeadScore.setCPT( newNoBackoffHeadScore )
    backoffHeadOnceScore.setCPT( newBackoffHeadOnceScore )
    backoffHeadTwiceScore.setCPT( newBackoffHeadTwiceScore )

    rootChooseScore.setCPT( newRootChooseScore )


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

  override def getParams = {
    // VanillaDMVParameters(
    //   p_order,
    //   p_stop,
    //   p_choose
    // )
    DMVBayesianBackoffThreeStreamParameters(
      p_stop,
      p_choose,
      stopBackoffFirstInterpolationScore,
      stopBackoffSecondInterpolationScore,
      stopNoBackoffScore,
      stopBackoffOnceScore,
      stopBackoffTwiceScore,
      chooseBackoffHeadFirstInterpolationScore,
      chooseBackoffHeadSecondInterpolationScore,
      noBackoffHeadScore,
      backoffHeadOnceScore,
      backoffHeadTwiceScore,
      rootChooseScore
    )
  }

  override def emptyPartialCounts = new DMVBayesianBackoffThreeStreamPartialCounts(
    noBackoffAlpha,
    backoffOneAlpha,
    backoffTwoAlpha
  )

  // override def toString =
  //   super.toString +
  //     "Alphas:\n" +
  //     "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
  //     "\tbackoffOneAlpha: " + backoffOneAlpha + "\n" +
  //     "\tbackoffTwoAlpha: " + backoffTwoAlpha + "\n"


  override def toString =
    "DMVBayesianBackoffGrammar\n" + 
    "Alphas:\n" +
    "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
    "\tbackoffOneAlpha: " + backoffOneAlpha + "\n" +
    "\tbackoffTwoAlpha: " + backoffTwoAlpha + "\n" +
    "P_Stop (" + math.exp( p_stop.getDefault ) + "):\n" + p_stop +
    "P_Choose (" + math.exp( p_choose.getDefault ) + "):\n" + p_choose +
    "stopBackoffFirstInterpolationScore:\n" + stopBackoffFirstInterpolationScore + "\n" +
    "stopBackoffSecondInterpolationScore:\n" + stopBackoffSecondInterpolationScore + "\n" +
    "stopNoBackoffScore:\n" + stopNoBackoffScore + "\n" +
    "stopBackoffOnceScore:\n" + stopBackoffOnceScore + "\n" +
    "stopBackoffTwiceScore:\n" + stopBackoffTwiceScore + "\n" +
    "chooseBackoffHeadFirstInterpolationScore:\n" + chooseBackoffHeadFirstInterpolationScore + "\n" +
    "chooseBackoffHeadSecondInterpolationScore:\n" + chooseBackoffHeadSecondInterpolationScore + "\n" +
    "noBackoffHeadScore:\n" + noBackoffHeadScore + "\n" +
    "backoffHeadOnceScore:\n" + backoffHeadOnceScore + "\n" +
    "backoffHeadTwiceScore:\n" + backoffHeadTwiceScore + "\n"

}

