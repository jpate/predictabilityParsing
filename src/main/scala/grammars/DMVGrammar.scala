package predictabilityParsing.grammars

import collection.mutable.Map

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.util.Math

//class DMVGrammar( vocabulary:Set[ObservedLabel] ) {
abstract class AbstractDMVGrammar {//( vocabulary:Set[ObservedLabel] ) {


  //protected val p_order = new LogCPT( vocabulary + Root, dmv.attachmentOrder )
  protected val p_order = new LogCPT( Set[ObservedLabel](), dmv.attachmentOrder )

  //private val p_stop_keys = dmv.stopOrNotKeys( vocabulary )
  //protected val p_stop = new LogCPT( p_stop_keys, dmv.stopDecision )
  protected val p_stop = new LogCPT( Set[StopOrNot](), dmv.stopDecision )

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

  //private val p_choose_keys = dmv.chooseKeys( vocabulary )
  //protected val p_choose = new LogCPT( p_choose_keys, vocabulary+Root )
  protected val p_choose = new LogCPT( Set[ChooseArgument](), Set[ObservedLabel]() )//vocabulary+Root )

  def emptyPartialCounts = new DMVPartialCounts

  // this probably shouldn't be here.... maybe I can have a "bayesianGrammar" trait with this?
  var freeEnergy = Double.NegativeInfinity

  def getParams:DMVParameters
  def getVanillaParams = VanillaDMVParameters( p_order, p_stop, p_choose )
  def setParams[P<:DMVParameters]( parameters:P ) /*{
    p_order.setCPT( otherGram.p_order.getCPT )
    p_stop.setCPT( otherGram.p_stop.getCPT )
    p_choose.setCPT( otherGram.p_choose.getCPT )
  }*/

  def orderScore( word:ObservedLabel, pref:AttachmentOrder ) = p_order( word, pref )
  def stopScore( stopKey:StopOrNot, stopDecision:StopDecision ) = p_stop( stopKey, stopDecision )
  def chooseScore( chooseKey:ChooseArgument, arg:ObservedLabel ) = arg match{
    case root:AbstractRoot => Double.NegativeInfinity
    case _ => p_choose( chooseKey, arg )
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
    p_order.setCPT( otherP_order /*.getCPT*/ )
    p_stop.setCPT( otherP_stop /*.getCPT*/ )
    p_choose.setCPT( otherP_choose /*.getCPT*/ )
  }

  def laplaceSmooth( l:Double, vocab:Set[ObservedLabel] ) {
    val logSmooth = math.log( l )

    // println( "About to smooth p_stop" )
    // println( "keySet:\n\t" + p_stop.parents.mkString("","\n\t","\n\n" ) )
    ( p_stop.parents ++ dmv.rootlessStopOrNotKeys( vocab ) ).foreach{ key =>
      dmv.stopDecision.foreach{ decision =>
        //println( "\t"+ key + " --> " + decision + " by " + l )
        p_stop.setValue(
          key,
          decision,
          Math.sumLogProb(
            p_stop( key, decision ),
            logSmooth
          )
        )
      }
    }
    // println( "about to normalize" )
    // println( "keySet:\n\t" + p_stop.parents.mkString("","\n\t","\n\n" ) )
    p_stop.normalize
    // println( "dun dun dunnnnn" )
    // println( "keySet:\n\t" + p_stop.parents.mkString("","\n\t","\n\n" ) )
    // println( "[ " + p_stop( p_stop.parents.head )( Stop ) )

    ( p_choose.parents ++ dmv.rootlessChooseKeys( vocab ) ).foreach{ key =>
      vocab.foreach{ arg =>
        p_choose.setValue(
          key,
          arg,
          Math.sumLogProb(
            p_choose( key, arg ),
            logSmooth
          )
        )
      }
    }
    p_choose.normalize

    (p_order.parents ++ vocab ).foreach{ key =>
      p_order.setValue(
        key,
        LeftFirst,
        p_order(p_order.parents.head,LeftFirst)
      )
      p_order.setValue(
        key,
        RightFirst,
        p_order(p_order.parents.head,RightFirst)
      )
    }
  }

  def randomize( vocab:Set[_<:ObservedLabel] ):Unit = randomize( vocab, 15, 10 )
  def randomize( vocab:Set[_<:ObservedLabel], seed:Int ):Unit = randomize( vocab, seed, 10 )
  def randomize( vocab:Set[_<:ObservedLabel], seed:Int, centeredOn:Int ):Unit = {
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
      Double.NegativeInfinity
    )
    p_order.setValue(
      Root,
      RightFirst,
      0D
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

  def setUniform( vocab:Set[_<:ObservedLabel] ):Unit = {
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


  override def toString =
    "P_Order (" + math.exp( p_order.getDefault ) + "):\n" + p_order +
    "P_Stop (" + math.exp( p_stop.getDefault ) + "):\n" + p_stop +
    "P_Choose (" + math.exp( p_choose.getDefault ) + "):\n" + p_choose

}

class DMVGrammar extends AbstractDMVGrammar{
  def setParams[P<:DMVParameters]( parameters:P ) {
    val VanillaDMVParameters( newP_order, newP_stop, newP_choose) = parameters
    p_order.setCPT( newP_order /*.getCPT*/ )
    p_stop.setCPT( newP_stop /*.getCPT*/ )
    p_choose.setCPT( newP_choose /*.getCPT*/ )
    // p_stop.setDefault(
    //   Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_stop.parents.size )  )
    // )
    // p_choose.setDefault(
    //   Math.expDigamma( 0D ) - Math.expDigamma( math.log( p_choose.parents.size ) )
    // )
  }


  def getParams:DMVParameters = VanillaDMVParameters( p_order, p_stop, p_choose )
}

