package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBothBayesianBackoffPartialCounts
import predictabilityParsing.util.Math

class DMVBothBayesianBackoffGrammar(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70//,
    // these are specific backoff parameters. We don't actually use these, it's just convenient to
    // keep them around so we can print them out for closer inspection. In the event of memory or time
    // problems, cutting these out of the class definition is an easy first step.
  // stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  // headBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  // argBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
) extends DMVGrammar {

      // def this(
      //   noBackoffAlpha:Double,
      //   backoffAlpha:Double
      // ) = this(
      //   noBackoffAlpha,
      //   backoffAlpha,
      //   // these are specific backoff parameters
      //   stopBackoffScore = Log2dTable(
      //     Set[StopOrNot](),
      //     dmv.backoffDecision,
      //     Map[BackoffDecision,Double](
      //       NotBackoff -> {
      //         Math.expDigamma( math.log( noBackoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
      //       },
      //       Backoff -> {
      //         Math.expDigamma( math.log( backoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
      //       }
      //     )
      //   ),
      //   headBackoffScore = Log2dTable(
      //     Set[ChooseArgument](),
      //     dmv.backoffDecision,
      //     Map[BackoffDecision,Double](
      //       NotBackoff -> {
      //         Math.expDigamma( math.log( noBackoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
      //       },
      //       Backoff -> {
      //         Math.expDigamma( math.log( backoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
      //       }
      //     )
      //   ),
      //   argBackoffScore = Log2dTable(
      //     Set[ChooseArgument](),
      //     dmv.backoffDecision,
      //     Map[BackoffDecision,Double](
      //       NotBackoff -> {
      //         Math.expDigamma( math.log( noBackoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
      //       },
      //       Backoff -> {
      //         Math.expDigamma( math.log( backoffAlpha ) ) -
      //           Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha ) )
      //       }
      //     )
      //   )
      // )
      // def this() = this( 35, 70 ) // defaults inspired by Headden for use on wsj10


  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
  )

  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }


  override def setParams[P<:DMVParameters]( parameters:P ) {
    //val DMVBothBayesianBackoffParameters(
    val VanillaDMVParameters(
      otherP_order,
      otherP_stop,
      otherP_choose//,
      // otherStopBackoffScore,
      // otherHeadBackoffScore,
      // otherArgBackoffScore
    ) = parameters

    p_order.setCPT( otherP_order )
    p_stop.setCPT( otherP_stop )
    p_choose.setCPT( otherP_choose )
    // stopBackoffScore.setCPT( otherStopBackoffScore )
    // headBackoffScore.setCPT( otherHeadBackoffScore )
    // argBackoffScore.setCPT( otherArgBackoffScore )

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

  override def getParams =
    //DMVFullBayesianBackoffParameters(
    VanillaDMVParameters(
      p_order,
      p_stop,
      p_choose//,
      // stopBackoffScore,
      // headBackoffScore,
      // argBackoffScore
    )

  override def emptyPartialCounts = {
    new DMVBothBayesianBackoffPartialCounts(
      noBackoffAlpha,
      backoffAlpha//,
      // stopBackoffScore,
      // headBackoffScore,
      // argBackoffScore
    )
  }

  override def toString =
    super.toString +
      // "\nStopBackoffScore:\n" +
      //   stopBackoffScore +
      // "\nHeadBackoffScore:\n" +
      //   headBackoffScore +
      // "\nArgBackoffScore:\n" +
      //   argBackoffScore +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffAlpha: " + backoffAlpha + "\n"
}

