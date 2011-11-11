package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

class DMVGrammar( vocabulary:Set[ObservedLabel] ) {

  val p_order = new LogCPT( vocabulary + Root, dmv.attachmentOrder )

  private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
  val p_stop = new LogCPT( p_stop_keys, dmv.stopDecision )

  private val p_choose_keys = dmv.chooseKeys( vocabulary )
  val p_choose = new LogCPT( p_choose_keys, vocabulary+Root )

  def this(
    corpus:List[List[TimedObservedLabel]],
    rightFirst:Double = 0.75,
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0
  ) {
    this( corpus.flatMap{ _.map{ _.w } }.toSet )
    import predictabilityParsing.partialCounts.DMVPartialCounts
    val pc = new DMVPartialCounts

    val rightFirstScore = math.log( rightFirst )
    val cAttachScore = math.log( cAttach )
    val cStopScore = math.log( cStop )
    val cNotStopScore = math.log( cNotStop )
    val stopUniformityScore = math.log( stopUniformity )

    corpus.map{ s => s :+ FinalRoot( s.length )}.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>

        if( s(i).w == Root ) { // inefficient treatment of stopCounts but who cares
          (0 to (i-1)).foreach{ leftK =>
            // choose initialization
            pc.incrementChooseCounts(
              ChooseArgument( s(i).w, LeftAttachment ),
              s(leftK).w,
              0D
            )
          }

          pc.setStopCounts(
            StopOrNot( Root, LeftAttachment, true ),
            Stop,
            Double.NegativeInfinity
          )
          pc.setStopCounts(
            StopOrNot( Root, LeftAttachment, true ),
            NotStop,
            0D
          )

          pc.setStopCounts(
            StopOrNot( Root, LeftAttachment, false ),
            Stop,
            0D
          )
          pc.setStopCounts(
            StopOrNot( Root, LeftAttachment, false ),
            NotStop,
            Double.NegativeInfinity
          )


          pc.setStopCounts(
            StopOrNot( Root, RightAttachment, true ),
            Stop,
            0D
          )
          pc.setStopCounts(
            StopOrNot( Root, RightAttachment, true ),
            NotStop,
            Double.NegativeInfinity
          )
          pc.setStopCounts(
            StopOrNot( Root, RightAttachment, false ),
            Stop,
            0D
          )
          pc.setStopCounts(
            StopOrNot( Root, RightAttachment, false ),
            NotStop,
            Double.NegativeInfinity
          )

          // ((i+1) to (s.length-1)).foreach{ rightK =>
          //   pc.setChooseCounts(
          //     ChooseArgument( s(i).w, RightAttachment ),
          //     s(rightK).w,
          //     0D
          //   )
          // }
        } else {
          (0 to (i-1)).foreach{ leftK =>
            // choose initialization
            pc.incrementChooseCounts(
              ChooseArgument( s(i).w, LeftAttachment ),
              s(leftK).w,
              0D - math.log( i - leftK ) + cAttachScore
            )
          }

          // to s.length-2 because we don't take Root as argument
          ((i+1) to (s.length-2)).foreach{ rightK =>
            pc.incrementChooseCounts(
              ChooseArgument( s(i).w, RightAttachment ),
              s(rightK).w,
              0D - math.log( rightK - i ) + cAttachScore
            )
          }

          // stop initialization
          if( i == 0 )
            pc.incrementStopCounts(
              StopOrNot( s(i).w, LeftAttachment, true ),
              Stop,
              cStopScore
            )
          else
            pc.incrementStopCounts(
              StopOrNot( s(i).w, LeftAttachment, true ),
              NotStop,
              cNotStopScore
            )

          if( i == (s.length-2) )
            pc.incrementStopCounts(
              StopOrNot( s(i).w, RightAttachment, true ),
              Stop,
              cStopScore
            )
          else
            pc.incrementStopCounts(
              StopOrNot( s(i).w, RightAttachment, true ),
              NotStop,
              cNotStopScore
            )

          if( i == 1 )
            pc.incrementStopCounts(
              StopOrNot( s(i).w, LeftAttachment, false ),
              Stop,
              cStopScore
            )
          else
            pc.incrementStopCounts(
              StopOrNot( s(i).w, LeftAttachment, false ),
              NotStop,
              cNotStopScore
            )

          if( i == (s.length-3) )
            pc.incrementStopCounts(
              StopOrNot( s(i).w, RightAttachment, false ),
              Stop,
              cStopScore
            )
          else
            pc.incrementStopCounts(
              StopOrNot( s(i).w, RightAttachment, false ),
              NotStop,
              cNotStopScore
            )
        }

        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, rightFirstScore )
        pc.setOrderCounts( s(i).w, LeftFirst, Math.subtractLogProb( 0D, rightFirstScore ) )
      }

    }

    // uniformness smoothing for choose
    // pc.chooseCounts.parents.foreach{ chooseKey =>
    //   if( chooseKey.h != Root )
    //     pc.chooseCounts(chooseKey).keySet.foreach{ w =>
    //       pc.incrementChooseCounts(
    //         chooseKey,
    //         w,
    //         cAttachScore
    //       )
    //     }
    // }

    // uniformness smoothing for stop
    pc.stopCounts.parents.foreach{ stopKey =>
      if( stopKey.w != Root ) {
        pc.incrementStopCounts( stopKey, Stop, stopUniformityScore )
        pc.incrementStopCounts( stopKey, NotStop, stopUniformityScore )
      }
    }

    setParams( pc.toDMVGrammar )
  }

  /*
  def this(
    corpus:List[TimedSentence],
    rightFirst:Double = 0.75,
    cAttach:Double = 1.0,
    cStop:Double = 1.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 1.0
  ) {
    this(
      corpus.map{ _.sentence},
      rightFirst,
      cAttach,
      cStop,
      cNotStop,
      stopUniformity
    )
  }
  */

  def setParams( otherGram:DMVGrammar ) {
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
  }

  def normalize {
    p_order.normalize
    p_stop.normalize
    p_choose.normalize
  }

  def setParams(
    otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
    otherP_stop:LogCPT[StopOrNot,StopDecision],
    otherP_choose:LogCPT[ChooseArgument,ObservedLabel]
  ) {
    p_order.setCPT( otherP_order.getCPT )
    p_stop.setCPT( otherP_stop.getCPT )
    p_choose.setCPT( otherP_choose.getCPT )
  }

  override def toString =
    "P_Order:\n" + p_order +
    "P_Stop:\n" + p_stop +
    "P_Choose:\n" + p_choose

}

