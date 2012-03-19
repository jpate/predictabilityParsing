package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffGrammar(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noStopBackoff:Double = 35,
  stopBackoff:Double = 70,
  noChooseBackoff:Double = 30,
  backoffHead:Double = 60,
  backoffArg:Double = 60,
  backoffBoth:Double = 120,
  // these are specific backoff parameters
  noStopBackoffScore:AbstractLog1dTable[StopOrNot],
  stopBackoffScore:AbstractLog1dTable[StopOrNot],
  noChooseBackoffScore:AbstractLog1dTable[ChooseArgument],
  backoffHeadScore:AbstractLog1dTable[ChooseArgument],
  backoffArgScore:AbstractLog1dTable[ChooseArgument],
  backoffBothScore:AbstractLog1dTable[ChooseArgument]
) extends DMVGrammar {

  //println( "Creating new DMVGrammar. The noStopBackoffScore is:\n" + noStopBackoffScore )

  //println( "Forming new grammar with noStopBackoffScore of:\n" + noStopBackoffScore )

  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
  )
  // println( "Setting stop default to: " + (
  //   Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  ) ) )
  // println( "Testing stop default: " +
  //   p_stop( StopOrNot( Word("yoyoyo"), RightAttachment, true ), Stop )
  // )
  // println( "Testing stop default again: " +
  //   stopScore( StopOrNot( Word("yoyoyo"), RightAttachment, true ), Stop )
  // )

  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =// p_order( word, pref )
    word match {
      case _:AbstractRoot => {
        pref match {
          case LeftFirst => 0D
          case RightFirst => Double.NegativeInfinity
        }
      }
      case _ => {
        pref match {
          case LeftFirst => Double.NegativeInfinity
          case RightFirst => 0D
        }
      }
    }

  // p_stop.setDefaultMap(
  //   Map[StopOrNot,Double]().withDefaultValue(
  //     Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size ) )
  //   )
  // )
  p_choose.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_choose.parents.size ) )
  )
  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size ) )
  )
  // p_choose.setDefaultMap(
  //   Map[ChooseArgument,Double]().withDefaultValue(
  //     Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_choose.parents.size ) )
  //   )
  // )

  def this(
    noStopBackoff:Double,
    stopBackoff:Double,
    noChooseBackoff:Double,
    backoffHead:Double,
    backoffArg:Double,
    backoffBoth:Double
  ) = this(
    noStopBackoff,
    stopBackoff,
    noChooseBackoff,
    backoffHead,
    backoffArg,
    backoffBoth,
    // these are specific backoff parameters
    noStopBackoffScore = Log1dTable(
      Set[StopOrNot](),
      math.log( noStopBackoff /(noStopBackoff + stopBackoff) )
    ),
    stopBackoffScore = Log1dTable(
      Set[StopOrNot](),
      math.log( stopBackoff /(noStopBackoff + stopBackoff) )
    ),
    noChooseBackoffScore = Log1dTable(
      Set[ChooseArgument](),
      math.log( noChooseBackoff/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffHeadScore = Log1dTable(
      Set[ChooseArgument](),
      math.log( backoffHead/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffArgScore = Log1dTable(
      Set[ChooseArgument](),
      math.log( backoffArg/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    ),
    backoffBothScore = Log1dTable(
      Set[ChooseArgument](),
      math.log( backoffBoth/(noChooseBackoff + backoffHead + backoffArg + backoffBoth) )
    )
  )
  def this() = this( 35, 70, 30, 60, 60, 120 )

  override def clearInterpolationScores {
    println( "clearing interpolation scores in the grammar..." )
    noStopBackoffScore.setPT(
      Log1dTable(
        Set[StopOrNot](),
        Math.expDigamma( math.log( noStopBackoff ) ) -
          Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      )//.getPT
    )
    stopBackoffScore.setPT(
      Log1dTable(
        Set[StopOrNot](),
        Math.expDigamma( math.log( stopBackoff ) ) -
          Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      )//.getPT
    )
    noChooseBackoffScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( noChooseBackoff ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    backoffHeadScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffHead ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    backoffArgScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffArg ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    backoffBothScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffBoth ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth) )
      )//.getPT
    )
  }

  override def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVBayesianBackoffParameters(
      //otherFreeEnergy,
      otherP_order,
      otherP_stop,
      otherP_choose,
      otherNoStopBackoffScore,
      otherStopBackoffScore,
      otherNoChooseBackoffScore,
      otherBackoffHeadScore,
      otherBackoffArgScore,
      otherBackoffBothScore
    ) = parameters

    // println(
    //   "setting parameters in DMVBayesianBackoffGrammar. otherNoStopBackoffScore is:\n" + otherNoStopBackoffScore )

    //freeEnergy = otherFreeEnergy
    p_order.setCPT( otherP_order )//.getCPT )
    p_stop.setCPT( otherP_stop )//.getCPT )
    p_choose.setCPT( otherP_choose )//.getCPT )
    noStopBackoffScore.setPT( otherNoStopBackoffScore )//.getPT )
    stopBackoffScore.setPT( otherStopBackoffScore )//.getPT )
    noChooseBackoffScore.setPT( otherNoChooseBackoffScore )//.getPT )
    backoffHeadScore.setPT( otherBackoffHeadScore )//.getPT )
    backoffArgScore.setPT( otherBackoffArgScore )//.getPT )
    backoffBothScore.setPT( otherBackoffBothScore )//.getPT )

    // println(
    //   "setting parameters in DMVBayesianBackoffGrammar. now noStopBackoffScore is:\n" +
    //   noStopBackoffScore )

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

  override def getParams = //VanillaDMVParameters( p_order, p_stop, p_choose )
    DMVBayesianBackoffParameters(
      //freeEnergy,
      p_order,
      p_stop,
      p_choose,
      noStopBackoffScore,
      stopBackoffScore,
      noChooseBackoffScore,
      backoffHeadScore,
      backoffArgScore,
      backoffBothScore
    )

  override def emptyPartialCounts = {
    //println( "Forming new partial counts with noStopBackoffScore:\n" + noStopBackoffScore )
    new DMVBayesianBackoffPartialCounts(
      noStopBackoff,
      stopBackoff,
      noChooseBackoff,
      backoffHead,
      backoffArg,
      backoffBoth,
      noStopBackoffScore,
      stopBackoffScore,
      noChooseBackoffScore,
      backoffHeadScore,
      backoffArgScore,
      backoffBothScore
    )
  }

  override def toString =
    super.toString +
      "\nnoStopBackoffScore (default val = " + math.exp( noStopBackoffScore.getDefault ) + "):\n" +
        noStopBackoffScore +
      "\nStopBackoffScore (default val = " + math.exp( stopBackoffScore.getDefault ) +  "):\n" +
        stopBackoffScore +
      "\nNoChooseBackoffScore (default = " + math.exp( noChooseBackoffScore.getDefault ) + "):\n" +
        noChooseBackoffScore +
      "\nBackoffHeadScore (default = " + math.exp( backoffHeadScore.getDefault ) + "):\n" +
        backoffHeadScore +
      "\nBackoffArgScore (default = " + math.exp( backoffArgScore.getDefault ) + "):\n" +
        backoffArgScore +
      "\nBackoffBothScore (default = " + math.exp( backoffBothScore.getDefault ) + "):\n" +
        backoffBothScore + "\n" +
      "Alphas:\n" +
      "\tnoStopBackoffAlpha: " + noStopBackoff + "\n" +
      "\tstopBackoffAlpha: " + stopBackoff + "\n" +
      "\tnoChooseBackoffAlpha: " + noChooseBackoff + "\n" +
      "\tbackoffHeadAlpha: " + backoffHead + "\n" +
      "\tbackoffArgAlpha: " + backoffArg + "\n" +
      "\tbackoffBothAlpha: " + backoffBoth + "\n"
}

