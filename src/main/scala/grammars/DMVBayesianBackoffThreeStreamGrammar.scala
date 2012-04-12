package predictabilityParsing.grammars

import collection.mutable.Map

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


  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
  )

  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =
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

  override def emptyPartialCounts = new DMVBayesianBackoffThreeStreamPartialCounts(
    noBackoffAlpha,
    backoffOneAlpha,
    backoffTwoAlpha
  )

  override def toString =
    super.toString +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffOneAlpha: " + backoffOneAlpha + "\n" +
      "\tbackoffTwoAlpha: " + backoffTwoAlpha + "\n"
}

