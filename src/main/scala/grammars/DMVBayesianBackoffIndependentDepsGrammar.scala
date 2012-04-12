package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffIndependentDepsPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffIndependentDepsGrammar(
    // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70
) extends DMVGrammar {


  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
  )

  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =// p_order( word, pref )
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }


  override def setParams[P<:DMVParameters]( parameters:P ) {
    val VanillaDMVParameters(
      otherP_order,
      otherP_stop,
      otherP_choose
    ) = parameters

    p_order.setCPT( otherP_order )
    p_stop.setCPT( otherP_stop )
    p_choose.setCPT( otherP_choose )

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
    VanillaDMVParameters(
      p_order,
      p_stop,
      p_choose
    )
  }

  override def emptyPartialCounts = new DMVBayesianBackoffIndependentDepsPartialCounts(
    noBackoffAlpha,
    backoffAlpha
  )

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffAlpha: " + backoffAlpha + "\n"
}

