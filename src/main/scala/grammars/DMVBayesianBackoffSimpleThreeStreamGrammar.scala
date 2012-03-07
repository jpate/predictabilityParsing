package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffSimpleThreeStreamPartialCounts
import predictabilityParsing.util.Math

class DMVBayesianBackoffSimpleThreeStreamGrammar(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noBackoff_Alpha:Double = 35,
  backoffW_Alpha:Double = 35,
  backoffA_Alpha:Double = 35,
  backoffWA_Alpha:Double = 35,
  backoffPA_Alpha:Double = 35,
  // these are specific backoff parameters
  noStopBackoff_Score:AbstractLog1dTable[ObservedLabel],
  stopBackoffW_Score:AbstractLog1dTable[ObservedLabel],
  stopBackoffA_Score:AbstractLog1dTable[ObservedLabel],
  stopBackoffWA_Score:AbstractLog1dTable[ObservedLabel],
  stopBackoffPA_Score:AbstractLog1dTable[ObservedLabel],
  noChooseBackoff_Score:AbstractLog1dTable[ObservedLabel],
  chooseBackoffW_Score:AbstractLog1dTable[ObservedLabel],
  chooseBackoffA_Score:AbstractLog1dTable[ObservedLabel],
  chooseBackoffWA_Score:AbstractLog1dTable[ObservedLabel],
  chooseBackoffPA_Score:AbstractLog1dTable[ObservedLabel]
) extends DMVGrammar {


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
    pref match {
      case LeftFirst => Double.NegativeInfinity
      case RightFirst => 0D
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
    noBackoff_Alpha:Double,
    backoffW_Alpha:Double,
    backoffA_Alpha:Double,
    backoffWA_Alpha:Double,
    backoffPA_Alpha:Double
  ) = this(
    noBackoff_Alpha,
    backoffW_Alpha,
    backoffA_Alpha,
    backoffWA_Alpha,
    backoffPA_Alpha,
    // these are specific backoff parameters
    noStopBackoff_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( noBackoff_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    stopBackoffW_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffW_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    stopBackoffA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    stopBackoffWA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffWA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    stopBackoffPA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffPA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    noChooseBackoff_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( noBackoff_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    chooseBackoffW_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffW_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    chooseBackoffA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    chooseBackoffWA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffWA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    ),
    chooseBackoffPA_Score = Log1dTable(
      Set[ObservedLabel](),
      Math.expDigamma( math.log( backoffPA_Alpha ) ) -
        Math.expDigamma( math.log(
            noBackoff_Alpha +
            backoffW_Alpha +
            backoffA_Alpha +
            backoffWA_Alpha +
            backoffPA_Alpha
        ) )
    )
  )

  def randomize( vocab:Set[ObservedLabelTriple] ):Unit = randomize( vocab, 15, 10 )
  def randomize( vocab:Set[ObservedLabelTriple], seed:Int ):Unit = randomize( vocab, seed, 10 )
  def randomize( vocab:Set[ObservedLabelTriple], seed:Int, centeredOn:Int ):Unit = {
    p_order.setCPTMap(
      Map(
        (vocab).map{ w =>
          w -> Map(
            LeftFirst -> Double.NegativeInfinity,
            RightFirst -> 0D
          )
        }.toSeq:_*
      )
    )
    p_order.setValue(
      Root,
      LeftFirst,
      0D
    )
    p_order.setValue(
      Root,
      RightFirst,
      Double.NegativeInfinity
    )
    p_stop.setCPTMap(
      Map(
        (
          dmv.rootlessStopOrNotKeys( vocab ) +
          StopOrNot( Root, LeftAttachment, false ) +
          StopOrNot( Root, LeftAttachment, true ) +
          StopOrNot( Root, RightAttachment, false ) +
          StopOrNot( Root, RightAttachment, true )
        ).map{ k =>
          k -> Map(
            dmv.stopDecision.map{ d =>
              d -> Double.NegativeInfinity
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
    p_stop.randomize( seed, centeredOn)
    p_choose.setCPTMap(
      Map(
        (
          dmv.rootlessChooseKeys( vocab ) +
          ChooseArgument( Root, LeftAttachment ) +
          ChooseArgument( Root, RightAttachment )
        ).map{ h =>
          h -> Map(
            vocab.map{ a:ObservedLabel =>
              a -> Double.NegativeInfinity
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )

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

    p_choose.randomize( seed, centeredOn)
  }

  def setUniform( vocab:Set[ObservedLabelTriple] ):Unit = {
    p_order.setCPTMap(
      Map(
        (vocab).map{ w =>
          w -> Map(
            LeftFirst -> Double.NegativeInfinity,
            RightFirst -> 0D
          )
        }.toSeq:_*
      )
    )
    p_order.setValue(
      Root,
      LeftFirst,
      0D
    )
    p_order.setValue(
      Root,
      RightFirst,
      Double.NegativeInfinity
    )
    p_stop.setCPTMap(
      Map(
        (
          dmv.rootlessStopOrNotKeys( vocab ) +
          StopOrNot( Root, LeftAttachment, false ) +
          StopOrNot( Root, LeftAttachment, true ) +
          StopOrNot( Root, RightAttachment, false ) +
          StopOrNot( Root, RightAttachment, true )
        ).map{ k =>
          k -> Map(
            dmv.stopDecision.map{ d =>
              d -> 0D
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
    p_stop.normalize
    p_choose.setCPTMap(
      Map(
        (
          dmv.rootlessChooseKeys( vocab ) +
          ChooseArgument( Root, LeftAttachment ) +
          ChooseArgument( Root, RightAttachment )
        ).map{ h =>
          h -> Map(
            vocab.map{ a:ObservedLabel =>
              a -> 0D
            }.toSeq:_*
          )
        }.toSeq:_*
      )
    )
    p_choose.normalize

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

      // override def clearInterpolationScores {
      //   println( "clearing interpolation scores in the grammar..." )
      //   noStopBackoffScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( noStopBackoff ) ) -
      //         Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      //     )//.getPT
      //   )
      //   stopBackoffScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( stopBackoff ) ) -
      //         Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
      //     )//.getPT
      //   )
      //   noChooseBackoffScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( noChooseBackoff ) ) -
      //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //     )//.getPT
      //   )
      //   backoffHeadScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( backoffHead ) ) -
      //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //     )//.getPT
      //   )
      //   backoffArgScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( backoffArg ) ) -
      //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
      //     )//.getPT
      //   )
      //   backoffBothScore.setPT(
      //     Log1dTable(
      //       Set[ObservedLabel](),
      //       Math.expDigamma( math.log( backoffBoth ) ) -
      //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth) )
      //     )//.getPT
      //   )
      // }

  override def setParams[P<:DMVParameters]( parameters:P ) {
    val DMVBayesianBackoffSimpleThreeStreamParameters(
      otherP_order,
      otherP_stop,
      otherP_choose,
      otherNoStopBackoff_Score,
      otherStopBackoffW_Score,
      otherStopBackoffA_Score,
      otherStopBackoffWA_Score,
      otherStopBackoffPA_Score,
      otherNoChooseBackoff_Score,
      otherChooseBackoffW_Score,
      otherChooseBackoffA_Score,
      otherChooseBackoffWA_Score,
      otherChooseBackoffPA_Score
    ) = parameters

    //freeEnergy = otherFreeEnergy
    p_order.setCPT( otherP_order )
    p_stop.setCPT( otherP_stop )
    p_choose.setCPT( otherP_choose )

    noStopBackoff_Score.setPT( otherNoStopBackoff_Score )
    stopBackoffW_Score.setPT( otherStopBackoffW_Score )
    stopBackoffA_Score.setPT( otherStopBackoffA_Score )
    stopBackoffWA_Score.setPT( otherStopBackoffWA_Score )
    stopBackoffPA_Score.setPT( otherStopBackoffPA_Score )

    noChooseBackoff_Score.setPT( otherNoChooseBackoff_Score )
    chooseBackoffW_Score.setPT( otherStopBackoffW_Score )
    chooseBackoffA_Score.setPT( otherStopBackoffA_Score )
    chooseBackoffWA_Score.setPT( otherStopBackoffWA_Score )
    chooseBackoffPA_Score.setPT( otherStopBackoffPA_Score )

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
    DMVBayesianBackoffSimpleThreeStreamParameters(
      p_order,
      p_stop,
      p_choose,
      noStopBackoff_Score,
      stopBackoffW_Score,
      stopBackoffA_Score,
      stopBackoffWA_Score,
      stopBackoffPA_Score,
      noChooseBackoff_Score,
      chooseBackoffW_Score,
      chooseBackoffA_Score,
      chooseBackoffWA_Score,
      chooseBackoffPA_Score
    )

  override def emptyPartialCounts = {
    //println( "Forming new partial counts with noStopBackoffScore:\n" + noStopBackoffScore )
    new DMVBayesianBackoffSimpleThreeStreamPartialCounts(
      noBackoff_Alpha,
      backoffW_Alpha,
      backoffA_Alpha,
      backoffWA_Alpha,
      backoffPA_Alpha,
      noStopBackoff_Score,
      stopBackoffW_Score,
      stopBackoffA_Score,
      stopBackoffWA_Score,
      stopBackoffPA_Score,
      noChooseBackoff_Score,
      chooseBackoffW_Score,
      chooseBackoffA_Score,
      chooseBackoffWA_Score,
      chooseBackoffPA_Score
    )
  }

  // override def toString =
  //   super.toString +
  //     "\nnoStopBackoffScore (default val = " + math.exp( noStopBackoffScore.getDefault ) + "):\n" +
  //       noStopBackoffScore +
  //     "\nStopBackoffScore (default val = " + math.exp( stopBackoffScore.getDefault ) +  "):\n" +
  //       stopBackoffScore +
  //     "\nNoChooseBackoffScore (default = " + math.exp( noChooseBackoffScore.getDefault ) + "):\n" +
  //       noChooseBackoffScore +
  //     "\nBackoffHeadScore (default = " + math.exp( backoffHeadScore.getDefault ) + "):\n" +
  //       backoffHeadScore +
  //     "\nBackoffArgScore (default = " + math.exp( backoffArgScore.getDefault ) + "):\n" +
  //       backoffArgScore +
  //     "\nBackoffBothScore (default = " + math.exp( backoffBothScore.getDefault ) + "):\n" +
  //       backoffBothScore + "\n" +
  //     "Alphas:\n" +
  //     "\tnoStopBackoffAlpha: " + noStopBackoff + "\n" +
  //     "\tstopBackoffAlpha: " + stopBackoff + "\n" +
  //     "\tnoChooseBackoffAlpha: " + noChooseBackoff + "\n" +
  //     "\tbackoffHeadAlpha: " + backoffHead + "\n" +
  //     "\tbackoffArgAlpha: " + backoffArg + "\n" +
  //     "\tbackoffBothAlpha: " + backoffBoth + "\n"
}

