package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVBayesianBackoffPartialCounts

class DMVBayesianBackoffGrammar(
  // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
  // decisions are drawn
  noChooseBackoff:Double = 30,
  backoffArg:Double = 60,
  backoffBoth:Double = 120,
  noStopBackoff:Double = 35,
  stopBackoff:Double = 70,
  // these are specific backoff parameters
  noChooseBackoffScore:AbstractLog1dTable[ObservedLabel],
  backoffArgScore:AbstractLog1dTable[ObservedLabel],
  backoffBothScore:AbstractLog1dTable[ObservedLabel],
  noStopBackoffScore:AbstractLog1dTable[ObservedLabel],
  stopBackoffScore:AbstractLog1dTable[ObservedLabel]
) extends DMVGrammar {

  def this(
    noChooseBackoff:Double,
    backoffArg:Double,
    backoffBoth:Double,
    noStopBackoff:Double,
    stopBackoff:Double
  ) = this(
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    noStopBackoff,
    stopBackoff,
    // these are specific backoff parameters
    noChooseBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( noChooseBackoff/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffArgScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( backoffArg/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    backoffBothScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( backoffBoth/(noChooseBackoff + backoffArg + backoffBoth) )
    ),
    noStopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( noStopBackoff /(noStopBackoff + stopBackoff) )
    ),
    stopBackoffScore = Log1dTable(
      Set[ObservedLabel](),
      math.log( stopBackoff /(noStopBackoff + stopBackoff) )
    )
  )
  def this() = this( 30, 60, 120, 35, 70)

  def randomize( vocab:Set[ObservedLabelPair] ):Unit = randomize( vocab, 15, 10 )
  def randomize( vocab:Set[ObservedLabelPair], seed:Int ):Unit = randomize( vocab, seed, 10 )
  def randomize( vocab:Set[ObservedLabelPair], seed:Int, centeredOn:Int ):Unit = {
    p_order.setCPT(
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
    p_stop.setCPT(
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
    p_choose.setCPT(
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
    p_choose.randomize( seed, centeredOn)
  }

  override def emptyPartialCounts = new DMVBayesianBackoffPartialCounts(
    noChooseBackoff,
    backoffArg,
    backoffBoth,
    noStopBackoff,
    stopBackoff,
    noChooseBackoffScore,
    backoffArgScore,
    backoffBothScore,
    noStopBackoffScore,
    stopBackoffScore
  )
}

