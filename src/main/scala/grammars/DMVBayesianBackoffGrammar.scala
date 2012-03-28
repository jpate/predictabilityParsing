package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffGrammar(
        // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
        // decisions are drawn
        // noStopBackoff:Double = 35,
        // stopBackoff:Double = 70,
        // noChooseBackoff:Double = 30,
        // backoffHead:Double = 60,
        // backoffArg:Double = 60,
        // backoffBoth:Double = 120,
        // // these are specific backoff parameters
        // noStopBackoffScore:AbstractLog1dTable[StopOrNot],
        // stopBackoffScore:AbstractLog1dTable[StopOrNot],
        // noChooseBackoffScore:AbstractLog1dTable[ChooseArgument],
        // backoffHeadScore:AbstractLog1dTable[ChooseArgument],
        // backoffArgScore:AbstractLog1dTable[ChooseArgument],
        // backoffBothScore:AbstractLog1dTable[ChooseArgument]
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noBackoffAlpha:Double = 35,
  backoffAlpha:Double = 70,
  // these are specific backoff parameters
  stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  headBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]//,
  //argBackoffScore:AbstractLog2dTable[WordPair,BackoffDecision]
) extends DMVGrammar {

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
    )//,
    // argBackoffScore = Log2dTable(
    //   Set[WordPair](),
    //   dmv.backoffDecision,
    //   Map[BackoffDecision,Double](
    //     Backoff -> {
    //       Math.expDigamma( math.log( backoffAlpha ) ) -
    //         Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
    //     },
    //     NotBackoff -> {
    //       Math.expDigamma( math.log( backoffAlpha ) ) -
    //         Math.expDigamma( math.log( noBackoffAlpha + backoffAlpha) )
    //     }
    //   )
    // )
  )
  def this() = this( 35, 70 ) // defaults inspired by Headden for use on wsj10


  //println( "Forming new grammar with noStopBackoffScore of:\n" + noStopBackoffScore )

  p_stop.setDefault(
    Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
  )

  override def orderScore( word:ObservedLabel, pref:AttachmentOrder ) =// p_order( word, pref )
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
    }

      // p_stop.setDefaultMap(
      //   Map[StopOrNot,Double]().withDefaultValue(
      //     Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size ) )
      //   )
      // )
      // p_choose.setDefault(
      //   Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_choose.parents.size ) )
      // )
      // p_stop.setDefault(
      //   Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size ) )
      // )
      // p_choose.setDefaultMap(
      //   Map[ChooseArgument,Double]().withDefaultValue(
      //     Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_choose.parents.size ) )
      //   )
      // )

      // def this(
      //   noStopBackoff:Double,
      //   stopBackoff:Double,
      //   noChooseBackoff:Double,
      //   backoffHead:Double,
      //   backoffArg:Double,
      //   backoffBoth:Double
      // ) = this(
      //   noStopBackoff,
      //   stopBackoff,
      //   noChooseBackoff,
      //   backoffHead,
      //   backoffArg,
      //   backoffBoth,
      //   // these are specific backoff parameters
      //   noStopBackoffScore = Log1dTable(
      //     Set[StopOrNot](),
      //     math.log( noStopBackoff /(noStopBackoff + stopBackoff) )
      //   ),
      //   stopBackoffScore = Log1dTable(
      //     Set[StopOrNot](),
      //     math.log( stopBackoff /(noStopBackoff + stopBackoff) )
      //   ),
      //   noChooseBackoffScore = Log1dTable(
      //     Set[ChooseArgument](),
      //     math.log( noChooseBackoff/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //   ),
      //   backoffHeadScore = Log1dTable(
      //     Set[ChooseArgument](),
      //     math.log( backoffHead/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //   ),
      //   backoffArgScore = Log1dTable(
      //     Set[ChooseArgument](),
      //     math.log( backoffArg/(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //   ),
      //   backoffBothScore = Log1dTable(
      //     Set[ChooseArgument](),
      //     math.log( backoffBoth/(noChooseBackoff + backoffHead + backoffArg + backoffBoth) )
      //   )
      // )
      // def this() = this( 35, 70, 30, 60, 60, 120 )

  /*
  override def clearInterpolationScores {
    println( "clearing interpolation scores in the grammar..." )
    stopBackoffScore.setPT(
      Log1dTable(
        Set[StopOrNot](),
        Math.expDigamma( math.log( backoffAlpha ) ) -
          Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      )//.getPT
    )
    headBackoffScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffHead ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
    argBackoffScore.setPT(
      Log1dTable(
        Set[ChooseArgument](),
        Math.expDigamma( math.log( backoffArg ) ) -
          Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      )//.getPT
    )
  }
  */

  override def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVBayesianBackoffParameters(
      otherP_order,
      otherP_stop,
      otherP_choose,
      otherStopBackoffScore,
      otherBackoffHeadScore
    ) = parameters

    p_order.setCPT( otherP_order )
    p_stop.setCPT( otherP_stop )
    p_choose.setCPT( otherP_choose )
    stopBackoffScore.setCPT( otherStopBackoffScore )
    headBackoffScore.setCPT( otherBackoffHeadScore )

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
      stopBackoffScore,
      headBackoffScore
    )

  override def emptyPartialCounts = {
    //println( "Forming new partial counts with noStopBackoffScore:\n" + noStopBackoffScore )
    new DMVBayesianBackoffPartialCounts(
      noBackoffAlpha,
      backoffAlpha,
      stopBackoffScore,
      headBackoffScore
    )
  }

  override def toString =
    super.toString +
      "\nStopBackoffScore:\n" +
        stopBackoffScore +
      "\nHeadBackoffScore:\n" +
        headBackoffScore +
      "Alphas:\n" +
      "\tnoBackoffAlpha: " + noBackoffAlpha + "\n" +
      "\tbackoffAlpha: " + backoffAlpha + "\n"
}

