package predictabilityParsing.parsers

// import scalala.library.Numerics.logSum
import predictabilityParsing.grammars.AbstractDMVGrammar
import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math.logSum
import math.{log,exp}


class VanillaDMVEstimator extends AbstractDMVParser{
  val g:AbstractDMVGrammar = new DMVGrammar//( vocabulary = vocab )

  //def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

  def setHardlineStopHarmonicGrammar[O<:TimedObservedLabel](
    corpus:List[List[O]],
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0,
    uniformRoot:Boolean = false
  ) {
    val pc = g.emptyPartialCounts
    println( "Hardline Stop Harmonic Grammar" )

    val cAttachScore = math.log( cAttach )
    val cStopScore = math.log( cStop )
    val cNotStopScore = math.log( cNotStop )
    val stopUniformityScore = math.log( stopUniformity )

    corpus/*.map{ s => s :+ FinalRoot( s.length )}*/.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>
        (0 to (i-1)).foreach{ leftK =>
          // choose initialization
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, LeftAttachment ),
            s(leftK).w,
            logSum(
              0D - math.log( i - leftK ) , cAttachScore
            )
          )
        }

        ((i+1) to (s.length-1)).foreach{ rightK =>
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, RightAttachment ),
            s(rightK).w,
            logSum(
              0D - math.log( rightK - i ) , cAttachScore
            )
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

        if( i == (s.length-1) )
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

        if( i == (s.length-2) )
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

        if( uniformRoot )
          pc.setChooseCounts(
            ChooseArgument( Root, LeftAttachment ),
            s(i).w,
            0D
          )
        else
          pc.incrementChooseCounts(
            ChooseArgument( Root, LeftAttachment ),
            s(i).w,
            cAttachScore
          )
        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, 0D )
        pc.setOrderCounts( s(i).w, LeftFirst, Double.NegativeInfinity )
      }

    }


    // uniformness smoothing for stop
    pc.stopCounts.parents.foreach{ stopKey =>
      pc.incrementStopCounts( stopKey, Stop, stopUniformityScore )
      pc.incrementStopCounts( stopKey, NotStop, stopUniformityScore )
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


    pc.setOrderCounts(
      Root,
      LeftFirst,
      Double.NegativeInfinity
    )
    pc.setOrderCounts(
      Root,
      RightFirst,
      0D
    )

    //pc.clearInterpolationScores
    println( "VanillaDMVEstimator.setHardlineStopHarmonicGrammar: running toDMVGrammar" )
    val newGrammar = pc.toDMVGrammar()
    println( "VanillaDMVEstimator.setHardlineStopHarmonicGrammar: done running toDMVGrammar" )

    //newGrammar.clearInterpolationScores
    println( "setting harmonic initialization:" )
    setGrammar( newGrammar )
  }


  def getHardlineStopHarmonicPartialCounts[O<:TimedObservedLabel](
    corpus:List[List[O]],
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0
  ) = {
    val pc = g.emptyPartialCounts
    println( "Hardline Stop Harmonic Grammar" )

    val cAttachScore = math.log( cAttach )
    val cStopScore = math.log( cStop )
    val cNotStopScore = math.log( cNotStop )
    val stopUniformityScore = math.log( stopUniformity )

    corpus/*.map{ s => s :+ FinalRoot( s.length )}*/.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>
        (0 to (i-1)).foreach{ leftK =>
          // choose initialization
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, LeftAttachment ),
            s(leftK).w,
            logSum(
              0D - math.log( i - leftK ) , cAttachScore
            )
          )
        }

        ((i+1) to (s.length-1)).foreach{ rightK =>
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, RightAttachment ),
            s(rightK).w,
            logSum(
              0D - math.log( rightK - i ) , cAttachScore
            )
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

        if( i == (s.length-1) )
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

        if( i == (s.length-2) )
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

        pc.incrementChooseCounts(
          ChooseArgument( Root, LeftAttachment ),
          s(i).w,
          cAttachScore
        )
        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, 0D )
        pc.setOrderCounts( s(i).w, LeftFirst, Double.NegativeInfinity )
      }

    }


    // uniformness smoothing for stop
    pc.stopCounts.parents.foreach{ stopKey =>
      pc.incrementStopCounts( stopKey, Stop, stopUniformityScore )
      pc.incrementStopCounts( stopKey, NotStop, stopUniformityScore )
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


    pc.setOrderCounts(
      Root,
      LeftFirst,
      Double.NegativeInfinity
    )
    pc.setOrderCounts(
      Root,
      RightFirst,
      0D
    )

    pc

    //pc.clearInterpolationScores
    // println( "VanillaDMVEstimator.setHardlineStopHarmonicGrammar: running toDMVGrammar" )
    // val newGrammar = pc.toDMVGrammar()
    // println( "VanillaDMVEstimator.setHardlineStopHarmonicGrammar: done running toDMVGrammar" )

    // //newGrammar.clearInterpolationScores
    // println( "setting harmonic initialization:" )
    // setGrammar( newGrammar )
  }

  def setMismatchedHardlineStopHarmonicGrammar[O<:TimedObservedLabel](
    corpus:List[List[O]],
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0
  ) {
    val pc = g.emptyPartialCounts
    println( "Hardline Stop Harmonic Grammar" )

    val cAttachScore = math.log( cAttach )
    val cStopScore = math.log( cStop )
    val cNotStopScore = math.log( cNotStop )
    val stopUniformityScore = math.log( stopUniformity )

    corpus/*.map{ s => s :+ FinalRoot( s.length )}*/.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>
        (0 to (i-1)).foreach{ leftK =>
          // choose initialization
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, LeftAttachment ),
            s(leftK).w,
            logSum(
              0D - math.log( i - leftK ) , cAttachScore
            )
          )
        }

        ((i+1) to (s.length-1)).foreach{ rightK =>
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, RightAttachment ),
            s(rightK).w,
            logSum(
              0D - math.log( rightK - i ) , cAttachScore
            )
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

        pc.incrementChooseCounts(
          ChooseArgument( Root, LeftAttachment ),
          s(i).w,
          cAttachScore
        )
        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, 0D )
        pc.setOrderCounts( s(i).w, LeftFirst, Double.NegativeInfinity )
      }

    }


    // uniformness smoothing for stop
    pc.stopCounts.parents.foreach{ stopKey =>
      pc.incrementStopCounts( stopKey, Stop, stopUniformityScore )
      pc.incrementStopCounts( stopKey, NotStop, stopUniformityScore )
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


    pc.setOrderCounts(
      Root,
      LeftFirst,
      Double.NegativeInfinity
    )
    pc.setOrderCounts(
      Root,
      RightFirst,
      0D
    )

    //pc.clearInterpolationScores
    val newGrammar = pc.toDMVGrammar()
    //newGrammar.clearInterpolationScores
    println( "setting harmonic initialization:" )
    setGrammar( newGrammar )
  }

  def setGradedStopHarmonicGrammar[O<:TimedObservedLabel](
    corpus:List[List[O]],
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0
  ) {
    val pc = g.emptyPartialCounts //new DMVPartialCounts
    println( "Graded Stop Harmonic Grammar" )

    val cAttachScore = math.log( cAttach )
    val cStopScore = math.log( cStop )
    val cNotStopScore = math.log( cNotStop )
    val stopUniformityScore = math.log( stopUniformity )

    corpus/*.map{ s => s :+ FinalRoot( s.length )}*/.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>
        (0 to (i-1)).foreach{ leftK =>
          // choose initialization
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, LeftAttachment ),
            s(leftK).w,
            logSum(
              0D - math.log( i - leftK ) , cAttachScore
            )
          )
        }

        ((i+1) to (s.length-1)).foreach{ rightK =>
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, RightAttachment ),
            s(rightK).w,
            logSum(
              0D - math.log( rightK - i ) , cAttachScore
            )
          )
        }

        // stop initialization
        pc.incrementStopCounts(
          StopOrNot( s(i).w, RightAttachment, true ),
          Stop,
          logSum(
            0D - math.log( s.length - i ), cStopScore
          )
        )
        pc.incrementStopCounts(
          StopOrNot( s(i).w, RightAttachment, true ),
          NotStop,
          cNotStopScore
        )
        if( i < s.length-1 ) {
          pc.incrementStopCounts(
            StopOrNot( s(i).w, RightAttachment, false ),
            Stop,
            logSum(
              0D - math.log( s.length - i ), cStopScore
            )
          )
          pc.incrementStopCounts(
            StopOrNot( s(i).w, RightAttachment, false ),
            NotStop,
            cNotStopScore
          )
        }


        pc.incrementStopCounts(
          StopOrNot( s(i).w, LeftAttachment, true ),
          Stop,
          logSum(
            0D - math.log( i+1 ), cStopScore
          )
        )
        pc.incrementStopCounts(
          StopOrNot( s(i).w, LeftAttachment, true ),
          NotStop,
          cNotStopScore
        )
        if( i > 0 ) {
          pc.incrementStopCounts(
            StopOrNot( s(i).w, LeftAttachment, false ),
            Stop,
            logSum(
              0D - math.log( i+1 ), cStopScore
            )
          )
          pc.incrementStopCounts(
            StopOrNot( s(i).w, LeftAttachment, false ),
            NotStop,
            cNotStopScore
          )
        }



        pc.incrementChooseCounts(
          ChooseArgument( Root, LeftAttachment ),
          s(i).w,
          cAttachScore
        )
        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, 0D )
        pc.setOrderCounts( s(i).w, LeftFirst, Double.NegativeInfinity )
      }

    }

    // uniformness smoothing for stop
    pc.stopCounts.parents.foreach{ stopKey =>
      pc.incrementStopCounts( stopKey, Stop, stopUniformityScore )
      pc.incrementStopCounts( stopKey, NotStop, stopUniformityScore )
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


    pc.setOrderCounts(
      Root,
      LeftFirst,
      Double.NegativeInfinity
    )
    pc.setOrderCounts(
      Root,
      RightFirst,
      0D
    )

    //pc.clearInterpolationScores
    val newGrammar = pc.toDMVGrammar()
    //newGrammar.clearInterpolationScores
    println( "setting harmonic initialization:" )
    setGrammar( newGrammar )
  }

  def setUniformStopHarmonicGrammar[O<:TimedObservedLabel](
    corpus:List[List[O]],
    cAttach:Double = 15.0
  ) {
    val pc = g.emptyPartialCounts //new DMVPartialCounts
    println( "Uniform Stop Harmonic Grammar" )

    val cAttachScore = math.log( cAttach )

    corpus/*.map{ s => s :+ FinalRoot( s.length )}*/.foreach{ s =>
      (0 to (s.length-1)).foreach{ i =>
        (0 to (i-1)).foreach{ leftK =>
          // choose initialization
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, LeftAttachment ),
            s(leftK).w,
            logSum(
              0D - math.log( i - leftK ) , cAttachScore
            )
          )
        }

        ((i+1) to (s.length-1)).foreach{ rightK =>
          pc.incrementChooseCounts(
            ChooseArgument( s(i).w, RightAttachment ),
            s(rightK).w,
            logSum(
              0D - math.log( rightK - i ) , cAttachScore
            )
          )
        }

        // stop initialization
        pc.setStopCounts(
          StopOrNot( s(i).w, RightAttachment, true ),
          Stop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, RightAttachment, true ),
          NotStop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, RightAttachment, false ),
          Stop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, RightAttachment, false ),
          NotStop,
          0D
        )


        pc.setStopCounts(
          StopOrNot( s(i).w, LeftAttachment, true ),
          Stop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, LeftAttachment, true ),
          NotStop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, LeftAttachment, false ),
          Stop,
          0D
        )
        pc.setStopCounts(
          StopOrNot( s(i).w, LeftAttachment, false ),
          NotStop,
          0D
        )



        pc.incrementChooseCounts(
          ChooseArgument( Root, LeftAttachment ),
          s(i).w,
          cAttachScore
        )
        // order initialization
        pc.setOrderCounts( s(i).w, RightFirst, 0D )
        pc.setOrderCounts( s(i).w, LeftFirst, Double.NegativeInfinity )
      }

    }


    // root attachment
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


    pc.setOrderCounts(
      Root,
      LeftFirst,
      Double.NegativeInfinity
    )
    pc.setOrderCounts(
      Root,
      RightFirst,
      0D
    )

    //pc.clearInterpolationScores
    val newGrammar = pc.toDMVGrammar()
    //newGrammar.clearInterpolationScores
    println( "setting harmonic initialization:" )
    setGrammar( newGrammar )
  }


  class Chart( s:List[TimedObservedLabel] ) {
    protected val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,Entry]()
    )

    def apply( start:Int, end:Int ) = matrix(start)(end)


    case class SpannedChildren( head:Entry, dependent:Option[Entry] )
    case class TwoParses( constituencyParse:String, dependencyParse:Set[DirectedArc] )

    // Ok, stupidly simple entry class
    class Entry( val label:MarkedObservation, val span:Span ) {

      // println( "Inserting entry " + label + " into span " + span )

      var iScore:Double = label.mark match {
        case UnsealedLeftFirst =>
          if( span.start - span.end == 1 )
            g.orderScore( label.obs.w, LeftFirst )
          else
            Double.NegativeInfinity
        case UnsealedRightFirst =>
          if( span.end - span.start == 1 )
            g.orderScore( label.obs.w, RightFirst )
          else
            Double.NegativeInfinity
        case SealedRight => {
          // println( "initializing " + label + " in " + span )
          if( label.obs.t == span.start ) {
            g.stopScore( StopOrNot( label.obs.w, RightAttachment, adj( label.peel.head, span ) ), Stop ) +
              matrix(span.start)(span.end)( label.peel.head ).iScore
          } else {
            Double.NegativeInfinity
          }
        }
        case SealedLeft => {
          // println( "initializing " + label + " in " + span )
          if( label.obs.t+1 == span.end ) {
            g.stopScore( StopOrNot( label.obs.w, LeftAttachment, adj( label.peel.head, span ) ), Stop ) +
              matrix(span.start)(span.end)( label.peel.head ).iScore
          } else {
            Double.NegativeInfinity
          }
        }
        case Sealed => {
          // println( "initializing " + label + " in " + span )
          (label.peel.toSet ).foldLeft( Double.NegativeInfinity) { (x, peeledH ) =>
                // println(
                //   "factor: " + StopOrNot( label.obs.w, peeledH.attachmentDirection, adj( peeledH, span )
                //   ) + " " + Stop + " " +
                //   exp( g.stopScore( StopOrNot( label.obs.w, peeledH.attachmentDirection, adj( peeledH,
                //   span ) ), Stop ) ) + " " + span
                // )
            val base = 
              g.stopScore( StopOrNot( label.obs.w, peeledH.attachmentDirection, adj( peeledH, span ) ), Stop ) +
                matrix(span.start)(span.end)( peeledH ).iScore
            logSum(
              x,
              base
            )
          }
        }
      }

      var oScore = Double.NegativeInfinity
      var score = Double.NegativeInfinity
      def computeMarginal { score = oScore + iScore - treeScore }

      def incrementIScore( inc:Double ) { iScore = logSum( iScore, inc ) }
      def incrementOScore( inc:Double ) { oScore = logSum( oScore, inc ) }

      def setOScore( x:Double ) { oScore = x }

      var children = Set[SpannedChildren]()

      def addDependency( headEntry:Entry, argEntry:Entry ) {
        val h = headEntry.label
        assert( label == h )
        val a = argEntry.label

            // println( "attaching " + argEntry.label + " to " + headEntry.label + " in " + span )
            // println(
            //   "factor: " + StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) + " " +
            //   NotStop + " " +
            //   exp( g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) ,
            //   NotStop ) ) + " " + 
            //   span
            // )
            // println( "factor: " + ChooseArgument( h.obs.w, h.attachmentDirection ) + " " + a.obs.w +
            //   " " +
            //   exp( g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) ) + " " +
            //   span
            // )

        val inc = 
          g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) +
          g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) +
          argEntry.iScore +
          headEntry.iScore

        // if(
        //   ( label.mark == UnsealedRightFirst || label.mark == SealedRight ) &&
        //   ( a.obs.w != Root )
        // )
        //   println( "    attaching " + argEntry.label + " to " + label + " with score " + 
        //     g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) + " + " +
        //     g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) + " + " +
        //     argEntry.iScore + " + " +
        //     headEntry.iScore + " = " + inc )
        // else if ( inc > Double.NegativeInfinity ) 
        //   print( "shit; attaching " + argEntry.label + " to " + label + " with score " + inc )


        incrementIScore( inc )

        // if( !( iScore <= 0D ) || ( h.obs.w != Root && iScore == Double.NegativeInfinity ) ) {
        //   println( h + " --> " + a +", " + span + ", " + StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) + " , " +
        //     ChooseArgument( h.obs.w, h.attachmentDirection ) + " , " + a.obs.w + "\n  " + 
        //     g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) + " + " +
        //     g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) + " + " +
        //     argEntry.iScore + " + " +
        //     headEntry.iScore + " = " + inc + "\n\n"
        //   )
        // }
        children += SpannedChildren( headEntry, Some(argEntry) )
      }

      def maxMarginalDependencyParse:Set[DirectedArc] = {
        val SpannedChildren( bestHead, bestArg ) = (
            if( label.sealCount > 0 )
              ( children ++
                ( label.peel.toSet & matrix(span.start)(span.end).keySet ) .map{ peeledLabel =>
                  SpannedChildren( matrix( span.start )( span.end )( peeledLabel ), None )
                }.toSet
              )
            else
              children
          ).reduce{ (currentBest,considering) =>
            val SpannedChildren( bestHead, bestArg ) = currentBest
            val SpannedChildren( newHead, newArg ) = considering
            if(
              {
                if( bestArg.isEmpty )
                  bestHead.score
                else
                  logSum( bestHead.score, bestArg.get.score )
              } > {
                if( newArg.isEmpty )
                  newHead.score
                else
                  logSum( newHead.score, newArg.get.score )
              }
            )
              currentBest
            else
              considering
          }

        bestHead.maxMarginalDependencyParse ++ {
          if( bestArg.isEmpty )
            Set[DirectedArc]()
          else
            bestArg.get.maxMarginalDependencyParse +
              DirectedArc( bestHead.label.obs, bestArg.get.label.obs )
          }


      }

      def maxMarginalConstituencyParse:String = {
        val SpannedChildren( bestHead, bestArg ) = (
            if( label.sealCount > 0 )
              ( children ++
                ( label.peel.toSet & matrix(span.start)(span.end).keySet ) .map{ peeledLabel =>
                  SpannedChildren( matrix( span.start )( span.end )( peeledLabel ), None )
                }.toSet
              )
            else
              children
          ).reduce{ (currentBest,considering) =>
            val SpannedChildren( bestHead, bestArg ) = currentBest
            val SpannedChildren( newHead, newArg ) = considering
            if(
              {
                if( bestArg.isEmpty )
                  bestHead.score
                else
                  logSum( bestHead.score, bestArg.get.score )
              } > {
                if( newArg.isEmpty )
                  newHead.score
                else
                  logSum( newHead.score, newArg.get.score )
              }
            )
              currentBest
            else
              considering
          }

        if( bestArg.isEmpty )
          "(" + label + " " + bestHead.maxMarginalConstituencyParse + " )"
        else
          if( bestHead.span.start < bestArg.get.span.start )
            "(" + label + " " +
              bestHead.maxMarginalConstituencyParse + " " +
              bestArg.get.maxMarginalConstituencyParse +
            " ) "
          else
            "(" + label + " " +
              bestArg.get.maxMarginalConstituencyParse +
              bestHead.maxMarginalConstituencyParse + " " +
            " ) "

      }

      def annotatedMaxMarginalConstituencyParse:String = {
        val SpannedChildren( bestHead, bestArg ) = (
            if( label.sealCount > 0 )
              ( children ++
                ( label.peel.toSet & matrix(span.start)(span.end).keySet ) .map{ peeledLabel =>
                  SpannedChildren( matrix( span.start )( span.end )( peeledLabel ), None )
                }.toSet
              )
            else
              children
          ).reduce{ (currentBest,considering) =>
            val SpannedChildren( bestHead, bestArg ) = currentBest
            val SpannedChildren( newHead, newArg ) = considering
            if(
              {
                if( bestArg.isEmpty )
                  bestHead.score
                else
                  logSum( bestHead.score, bestArg.get.score )
              } > {
                if( newArg.isEmpty )
                  newHead.score
                else
                  logSum( newHead.score, newArg.get.score )
              }
            )
              currentBest
            else
              considering
          }

        if( bestArg.isEmpty ) { // unary stop rule.
          val myScore =
            oScore +
            g.stopScore(
              StopOrNot( label.obs.w, bestHead.label.attachmentDirection, adj( bestHead.label, span ) ), Stop
            )+ bestHead.iScore
          "(" + label + ":" + myScore + " " + bestHead.annotatedMaxMarginalConstituencyParse + " )"
        } else { // binary attachment rule.
          if( bestHead.span.start < bestArg.get.span.start ) { // rightward attachment

            assert( bestHead.label.attachmentDirection == RightAttachment )
            val myScore =
              oScore +
              g.stopScore(
                StopOrNot( label.obs.w, RightAttachment, adj( bestHead.label, bestHead.span ) ), NotStop
              ) +
              g.chooseScore(
                ChooseArgument( label.obs.w, RightAttachment ), bestArg.get.label.obs.w
              ) +
              bestHead.iScore + bestArg.get.iScore

              "(" + label + ":" + myScore + " " +
                bestHead.annotatedMaxMarginalConstituencyParse + " " +
                bestArg.get.annotatedMaxMarginalConstituencyParse +
              " ) "

          } else { // leftward attachment

            assert( bestHead.label.attachmentDirection == LeftAttachment )
            val myScore =
              oScore +
              g.stopScore(
                StopOrNot( label.obs.w, LeftAttachment, adj( bestHead.label, bestHead.span ) ), NotStop
              ) +
              g.chooseScore(
                ChooseArgument( label.obs.w, LeftAttachment ), bestArg.get.label.obs.w
              ) +
              bestHead.iScore + bestArg.get.iScore


              "(" + label + ":" + myScore + " " +
                bestArg.get.annotatedMaxMarginalConstituencyParse +
                bestHead.annotatedMaxMarginalConstituencyParse + " " +
              " ) "
            }
        }

      }


      override def toString = 
        span + ": " + label +
          "\n  iScore: " + math.exp( iScore ) +
          "\n  oScore: " + math.exp( oScore ) +
          "\n  score: " +  math.exp( score  ) 

    }

    class TerminalEntry( h:MarkedObservation, index:Int ) extends Entry( h, Span( index, index +1 )
    ) {
      override def maxMarginalDependencyParse = Set[DirectedArc]()
      override def maxMarginalConstituencyParse = "(" + h + "  " + h.obs + ") "
      override def annotatedMaxMarginalConstituencyParse = "(" + h + ":" + score + "  " + h.obs + ") "
    }


    def rootEntry = matrix( 0 )( s.length )( MarkedObservation( FinalRoot(s.length-1), Sealed ) )
    def treeScore = rootEntry.iScore
      //matrix( 0 )( s.length )( MarkedObservation( FinalRoot( s.length-1 ), Sealed ) ).iScore

    def toMaxMarginalDependencyParse =
      rootEntry.maxMarginalDependencyParse.toList.sortWith{ _.arg.t < _.arg.t }.map{_.head.t}.mkString( "[ ", ", ", " ] " )
    def toMaxMarginalConstituencyParse = rootEntry.maxMarginalConstituencyParse

    def toAnnotatedMaxMarginalConstituencyParse = rootEntry.annotatedMaxMarginalConstituencyParse


    // Re-wrote lexFill so we don't explicitly touch iScore at all, only add entries.
    def lexFill( index:Int ) {
      //println( (index,index+1) )
      val w = s(index)

      // Add possible unsealed preterminals with attachment order preference.
      val unsealedLeftFirst = MarkedObservation( w, UnsealedLeftFirst )
      matrix( index )( index+1 ) +=
        unsealedLeftFirst -> new TerminalEntry( unsealedLeftFirst, index )

      val unsealedRightFirst = MarkedObservation( w, UnsealedRightFirst )
      matrix( index )( index+1 ) +=
        unsealedRightFirst -> new TerminalEntry( unsealedRightFirst, index )

      // Now add length-one span seals
      val sealedLeft = MarkedObservation( w, SealedLeft )
      matrix( index )( index+1 ) +=
        sealedLeft -> new TerminalEntry( sealedLeft, index )

      val sealedRight = MarkedObservation( w, SealedRight )
      matrix( index )( index+1 ) +=
        sealedRight -> new TerminalEntry( sealedRight, index )

      // Finally add length-one full seals
      val sealedBoth = MarkedObservation( w, Sealed )
      matrix( index )( index+1 ) +=
        sealedBoth -> new TerminalEntry( sealedBoth, index )
    }

    // Re-wrote synFill so we don't explicitly touch iScore at all, only add entries and dependents.
    def synFill( start:Int, end:Int ) {
      //println( (start,end) )
      // First, let's just add all the possible heads with possible dependencies. Since we are
      // guaranteed that end-start>1, we cannot have any seals until we have added heads to the
      // current span.

      ( (start+1) to (end-1) ).foreach{ k =>
        // println( "  " + (start,k,end) )
        val rightArguments = matrix(k)(end).keySet.filter{ _.mark == Sealed }
        val leftArguments = matrix(start)(k).keySet.filter{ _.mark == Sealed }

        val unsealedLeftHeads = matrix(start)(k).keySet.filter{ _.mark == UnsealedRightFirst }
        unsealedLeftHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) ) {
                // println( "Adding " + h + " to " + (start,end) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
          }
          rightArguments.foreach( a =>
            matrix(start)(end)(h).addDependency(
              matrix(start)(k)(h),
              matrix(k)(end)(a)
            )
          )
        }

        val unsealedRightHeads = matrix(k)(end).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) ) {
              // println( "Adding " + h + " to " + (start,end) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
          }

          leftArguments.foreach( a => 
            matrix(start)(end)(h).addDependency(
              matrix(k)(end)(h),
              matrix(start)(k)(a)
            )
          )
        }



        val halfSealedLeftHeads =
          matrix(start)(k).keySet.filter{ _.mark == SealedLeft }
        halfSealedLeftHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) ) {
                // println( "Adding " + h + " to " + (start,end) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
          }
          rightArguments.foreach( a =>
            matrix(start)(end)(h).addDependency(
              matrix(start)(k)(h),
              matrix(k)(end)(a)
            )
          )
        }

        val halfSealedRightHeads =
          matrix(k)(end).keySet.filter{ _.mark == SealedRight }
        halfSealedRightHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) ) {
                // println( "Adding " + h + " to " + (start,end) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
          }
          leftArguments.foreach( a =>
            matrix(start)(end)(h).addDependency(
              matrix(k)(end)(h),
              matrix(start)(k)(a)
            )
          )
        }

      }

      matrix(start)(end).keySet.filter{ h =>
        (h.mark == UnsealedLeftFirst ) | (h.mark == UnsealedRightFirst)
      }.foreach( h =>
        if( !( matrix(start)(end).keySet.contains( h.seal.get ) ) ) {
              // println( "Adding " + h + " to " + (start,end) )
          matrix(start)(end) += h.seal.get -> new Entry( h.seal.get, Span(start,end) )
        }
      )
      matrix(start)(end).keySet.filter{ h =>
        (h.mark == SealedLeft ) | (h.mark == SealedRight)
      }.foreach( h =>
        if( !( matrix(start)(end).keySet.contains( h.seal.get ) ) ) {
              // println( "Adding " + h + " to " + (start,end) )
          matrix(start)(end) += h.seal.get -> new Entry( h.seal.get, Span(start,end) )
        }
      )
    }

    def outsidePass {
      import math.exp
      val n = s.length

      rootEntry.setOScore( 0D )
      ( 1 to n ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>
          val j = i + length
          val curSpan = Span(i,j)

          // Compute from most-sealed to least-sealed.
          // So Sealed first.
          val curSpanSealedNodes = matrix(i)(j).keySet.filter{ _.mark == Sealed }
          curSpanSealedNodes.foreach{ a =>
            // Look both left and right for each sealed node. First, look left.
            // to (i-1) so we don't bother looking for possible heads in spans of length 0
            (0 to (i-1) ).foreach{ k =>
              val rightLookingHeads =
                matrix(k)(i).keySet.filter{ _.attachmentDirection == RightAttachment }

              rightLookingHeads.foreach{ h =>
                matrix(i)(j)(a).incrementOScore(
                  g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj(h, Span(k,i) ) ) , NotStop ) +
                  g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                  matrix( k )( i )( h ).iScore +
                  matrix( k )( j )( h ).oScore
                )
              }

            }


            // Now look right. Similarly, from j+1 so we don't bother looking for possible heads
            // in spans of length 1
            ( (j+1) to n ).foreach{ k =>
              val leftLookingHeads =
                matrix(j)(k).keySet.filter{ _.attachmentDirection == LeftAttachment }

              leftLookingHeads.foreach{ h =>
                matrix(i)(j)(a).incrementOScore(
                  g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj(h, Span(j,k) ) ) , NotStop ) +
                  g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                  matrix( j )( k )( h ).iScore +
                  matrix( i )( k )( h ).oScore
                )
              }

            }
          } // end sealed nodes

          // Now, half-sealed to the left
          val halfSealedLeft = matrix(i)(j).keySet.filter{ _.mark == SealedLeft }
          halfSealedLeft.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.stopScore( StopOrNot( w.obs.w, RightAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.stopScore( StopOrNot( w.obs.w, RightAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.chooseScore( ChooseArgument( w.obs.w, RightAttachment ) , a.obs.w ) + 
                      matrix(i)(k)(w).oScore + matrix(j)(k)(a).iScore
                )
              }
            }
          }

          // Now half-sealed to the right
          val halfSealedRight = matrix(i)(j).keySet.filter{ _.mark == SealedRight }
          halfSealedRight.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.stopScore(StopOrNot( w.obs.w, LeftAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.stopScore( StopOrNot( w.obs.w, LeftAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.chooseScore( ChooseArgument( w.obs.w, LeftAttachment ) , a.obs.w ) + 
                      matrix(k)(j)(w).oScore + matrix(k)(i)(a).iScore
                )
              }
            }
          }

          // On to unsealed. First, unsealed, left-first
          val unsealedLeftFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedLeftFirst }
          unsealedLeftFirst.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.stopScore( StopOrNot( w.obs.w, LeftAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.stopScore( StopOrNot( w.obs.w, LeftAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.chooseScore( ChooseArgument( w.obs.w, LeftAttachment ) , a.obs.w ) + 
                      matrix(k)(j)(w).oScore + matrix(k)(i)(a).iScore
                )
              }
            }
          }

          // Now, unsealed, right-first
          val unsealedRightFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedRightFirst }
          unsealedRightFirst.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.stopScore( StopOrNot( w.obs.w, RightAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.stopScore( StopOrNot( w.obs.w, RightAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.chooseScore( ChooseArgument( w.obs.w, RightAttachment ) , a.obs.w ) + 
                      matrix(i)(k)(w).oScore + matrix(j)(k)(a).iScore
                )
              }
            }
          }



        }
      )
    }


    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = g.emptyPartialCounts

      (0 to (s.length-1) ).foreach{ i =>
        // count up order preference:
        pc.incrementOrderCounts(
          s(i).w,
          RightFirst,
          matrix(i)(i+1)(MarkedObservation( s(i), UnsealedRightFirst ) ).score
        )
        pc.incrementOrderCounts(
          s(i).w,
          LeftFirst,
          matrix(i)(i+1)(MarkedObservation( s(i), UnsealedLeftFirst ) ).score
        )

        ( (i+1) to s.length ).foreach{ j =>
          val curSpan = Span( i, j )

          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            pc.incrementStopCounts(
              StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan ) ),
              Stop,
              matrix(i)(j)(h).iScore +
                g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ) , Stop ) +
                  matrix(i)(j)(h.seal.get).oScore -
                    treeScore
            )
          }

          // Ok, now increment choose counts. The re-estimated probability of an attachment is just
          // the product of not stopping, the previous estimate of the probability of attachment,
          // and the marginal score of the head. There's no need to keep track of the denominator
          // here; we can just sum for the numerator then normalize at the end.
          ( (i+1) to (j-1) ).foreach{ k =>

            val rightArguments = matrix(k)(j).keySet.filter{ _.mark == Sealed }
            matrix(i)(k).keySet.filter{ _.attachmentDirection == RightAttachment }.foreach{ h =>
              rightArguments.foreach{ a =>
                pc.incrementChooseCounts(
                  ChooseArgument( h.obs.w, RightAttachment ),
                  a.obs.w,
                  g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                      matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
                // if(!(pc.chooseCounts(ChooseArgument( h.obs.w, RightAttachment ), a.obs.w) <= 0D)) {
                //   println( "> " + 
                //     ChooseArgument( h.obs.w, RightAttachment ) + ", " +
                //     a.obs.w + ", " +
                //       g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop) + " + "  +
                //       g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) + " + " +
                //       matrix(i)(k)(h).iScore + " + " +
                //       matrix(k)(j)(a).iScore + " + " +
                //       matrix(i)(j)(h).oScore + " - " +
                //       treeScore + " = " +
                //     pc.chooseCounts(ChooseArgument( h.obs.w, RightAttachment ), a.obs.w)
                //   )
                // }

                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ),
                  NotStop,
                    g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                      g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                        matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore -
                         treeScore
                )

              }



            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                pc.incrementChooseCounts(
                  ChooseArgument( h.obs.w, LeftAttachment ),
                  a.obs.w,
                  g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )

                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ),
                  NotStop,
                  g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
              }
            }
          }

        }
      }

      if( !( treeScore <= 0D ) ) {
        println( s.mkString("[ ", " ", " ]" ) )
        println( treeScore )
        println( this )
      }
      assert( treeScore <= 0D )
      pc.setTotalScore( treeScore )

      // println( "This partial counts:\n" + pc )
      // println( " ---END PARTIAL COUNTS--- " )

      pc
    }

    def spanProbabilities = {
      // This is like toPartialCounts except we  distinguish different tokens of the same word, and
      // also keep track of arc probabilities in a local array of arrays.

      object Seal extends ObservedLabel( "SEAL" )
      case class TimedSeal(time:Int) extends TimedObservedLabel( Seal, time )

      case class DirectedSpanWithValence(
        head:TimedObservedLabel,
        //arg:TimedObservedLabel,
        dir:AttachmentDirection,
        s:Int,
        e:Int
        // Valence is redundant with the span of the head of the directed span
        // v:Boolean
      ) extends HiddenLabel( head + " " + dir + " " + s + " -- " + e ) {
        def <( a2:DirectedSpanWithValence) =
          if( s < a2.s )
            true
          else if( s > a2.s )
            false
          else if( e < a2.e )
            true
          else
            false

        def >( a2:DirectedSpanWithValence) =
          if( s > a2.s )
            true
          else if( s > a2.s )
            false
          else if( e > a2.e )
            true
          else
            false
      }


      //val allArcs = collection.mutable.Map[DirectedSpanWithValence,Double]().withDefaultValue( Double.NegativeInfinity )
      val allArcs = new predictabilityParsing.types.tables.Log2dTable(
      Set[DirectedSpanWithValence](), Set[TimedObservedLabel]() )
      (0 to (s.length-1) ).foreach{ i =>

        ( (i+1) to ( s.length ) ).foreach{ j =>
          val curSpan = Span( i, j )

          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            val latentVar = DirectedSpanWithValence(
              h.obs,
              h.attachmentDirection,
              if( h.attachmentDirection == RightAttachment ) h.obs.t else i,
                                                            // +1 b/c words appear between indices
              if( h.attachmentDirection == LeftAttachment ) h.obs.t+1 else j
            )

            val thisSeal = if( h.attachmentDirection == RightAttachment ) TimedSeal( j ) else TimedSeal( i )

            if(
              !(  // Don't bother sealing Root to the right
                h.attachmentDirection == RightAttachment && h.obs.w.toString == "--Root--"
              ) && !( // don't bother with second seals of Root to the left
                h.obs.w.toString == "--Root--" && (latentVar.e - latentVar.s > 1 )
              ) && !( // don't bother sealing the initial word to the left
                h.attachmentDirection == LeftAttachment && latentVar.s == 0
              ) && !( // finally, don't bother sealing the final word to the right (-1 b/c of Root)
                h.attachmentDirection == RightAttachment && latentVar.e >= (s.length-1)
              )
            )
              allArcs.setValue(
                latentVar,
                thisSeal,
                logSum(
                  allArcs( latentVar, thisSeal ),
                  matrix(i)(j)(h).iScore +
                    g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ) , Stop ) +
                      matrix(i)(j)(h.seal.get).oScore -
                        treeScore
                )
              )
          }

          ( (i+1) to (j-1) ).foreach{ k =>

            val rightArguments = matrix(k)(j).keySet.filter{ _.mark == Sealed }
            matrix(i)(k).keySet.filter{ _.attachmentDirection == RightAttachment }.foreach{ h =>
              rightArguments.foreach{ a =>
                val thisArc =
                  DirectedSpanWithValence(
                    h.obs,
                    RightAttachment,
                    h.obs.t,
                    k
                  )

                // Root should never take a rightward argument or be an argument
                if( h.obs.w.toString != "--Root--" && a.obs.w.toString != "--Root--" ) {
                  allArcs.setValue(
                    thisArc,
                    a.obs,
                    logSum(
                      allArcs( thisArc, a.obs ),
                      g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                        g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                          matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore -
                            treeScore
                    )
                  )
                }
              }

            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                val thisArc =
                  DirectedSpanWithValence(
                    h.obs,
                    LeftAttachment,
                    k,
                    h.obs.t+1
                  )

                if( !(
                  // Root should never take a second leftward argument
                  h.obs.w.toString == "--Root--" && ( thisArc.e - thisArc.s ) > 1
                ) ) {
                  allArcs.setValue(
                    thisArc,
                    a.obs,
                    logSum(
                      allArcs( thisArc, a.obs ),
                      g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                        g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                          matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore -
                            treeScore
                    )
                  )
                }
              }
            }

          }
        }
      }

      allArcs.normalize
      allArcs.parents.toList.sortWith{ _ < _ }.map{ case DirectedSpanWithValence( h, dir, s, e ) =>
        Map(
          "marginals" -> allArcs( DirectedSpanWithValence( h, dir, s, e ) ).keys.toList.map{ d =>
            List(
              s,
              e,
              h.t,
              d.t,
              h.w,
              d.w,
              dir,
              allArcs( DirectedSpanWithValence( h, dir, s, e ), d )
            ).mkString("",",","") // UGLY UGLY UGLY BAD BAD JOHN NO COOKIE FOR YOU
          },
          "entropy" -> List(
            s,
            e,
            h.t,
            h.w,
            dir,
            allArcs( DirectedSpanWithValence( h, dir, s, e ) ).keys.filterNot{ _.toString == "--Root--" }.map{ d =>
              val x = allArcs( DirectedSpanWithValence( h, dir, s, e ), d )
              if( x > Double.NegativeInfinity ) -1 * math.exp( x ) * x/math.log(2) else 0D
            }.reduce(_+_)
          )
        )
      }
    }

    def arcProbabilities = {
      // This is like toPartialCounts except we  distinguish different tokens of the same word, and
      // also keep track of arc probabilities in a local array of arrays.

      object Seal extends ObservedLabel( "SEAL" )
      case class TimedSeal(time:Int) extends TimedObservedLabel( Seal, time )

      case class DirectedArcWithValence( head:TimedObservedLabel,
      arg:TimedObservedLabel, v:Boolean )
        extends HiddenLabel( head + " " + "-" +v+ "-> " + arg ) {
        def <( a2:DirectedArcWithValence) =
          if( head.t < a2.head.t )
            true
          else if( head.t > a2.head.t )
            false
          else
            if( arg.t < a2.arg.t )
              true
            else
              false

        def >( a2:DirectedArcWithValence) =
          if( head.t > a2.head.t )
            true
          else if( head.t < a2.head.t )
            false
          else
            if( arg.t > a2.arg.t )
              true
            else
              false
      }


      val allArcs = collection.mutable.Map[DirectedArcWithValence,Double]().withDefaultValue( Double.NegativeInfinity )
      (0 to (s.length-1) ).foreach{ i =>

        ( (i+1) to ( s.length ) ).foreach{ j =>
          val curSpan = Span( i, j )

          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            val thisSeal =
              if( h.attachmentDirection == RightAttachment )
                DirectedArcWithValence( h.obs, TimedSeal( j ), adj( h, curSpan ) )
              else
                DirectedArcWithValence( h.obs, TimedSeal( i ), adj( h, curSpan ) )

            allArcs( thisSeal ) = logSum(
              allArcs( thisSeal ),
              matrix(i)(j)(h).iScore +
                g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ) , Stop ) +
                  matrix(i)(j)(h.seal.get).oScore -
                    treeScore
            )
          }

          ( (i+1) to (j-1) ).foreach{ k =>

            val rightArguments = matrix(k)(j).keySet.filter{ _.mark == Sealed }
            matrix(i)(k).keySet.filter{ _.attachmentDirection == RightAttachment }.foreach{ h =>
              rightArguments.foreach{ a =>
                val thisArc =
                  DirectedArcWithValence( h.obs, a.obs, adj( h, curSpan ) )

                allArcs( thisArc ) = logSum(
                  allArcs( thisArc ),
                  g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                      matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
              }

            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                val thisArc =
                  DirectedArcWithValence( h.obs, a.obs, adj( h, curSpan ) )

                allArcs( thisArc ) = logSum(
                  allArcs( thisArc ),
                  g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
              }
            }

          }
        }
      }

      allArcs.keys.toList.sortWith{ _ < _ }.map{ case DirectedArcWithValence( h,
      a, v ) =>
        List( h.t, a.t, h.w, a.w, v, allArcs( DirectedArcWithValence( h, a, v ) ) )
      }
    }

    def stopProbabilities = {
      // This is like toPartialCounts except we  distinguish different tokens of the same word and
      // only track stop decisions (i.e. averaging over dependents)

      // object Seal extends ObservedLabel( "SEAL" )
      // case class TimedSeal(time:Int) extends TimedObservedLabel( Seal, time )
      case class StopEvent( w:TimedObservedLabel, dir: AttachmentDirection, adj:Boolean,
      dec:StopDecision ) {
        def <(s2:StopEvent) =
          if( w.w < s2.w.w )
            true
          else if( w.w > s2.w.w )
            false
          else
            if( w.t < s2.w.t )
              true
            else if( w.t > s2.w.t )
              false
            else
              if( dir < s2.dir )
                true
              else if( dir > s2.dir )
                false
              else
                if( adj < s2.adj )
                  true
                else if( adj > s2.adj )
                  false
                else
                  if( adj < s2.adj )
                    true
                  else if( adj > s2.adj )
                    false
                  else
                    if( dec == Stop )
                      true
                    else
                      false

        def >(s2:StopEvent) =
          if( w.w > s2.w.w )
            true
          else if( w.w < s2.w.w )
            false
          else
            if( w.t > s2.w.t )
              true
            else if( w.t < s2.w.t )
              false
            else
              if( dir > s2.dir )
                true
              else if( dir < s2.dir )
                false
              else
                if( adj > s2.adj )
                  true
                else if( adj < s2.adj )
                  false
                else
                  if( adj > s2.adj )
                    true
                  else if( adj < s2.adj )
                    false
                  else
                    if( dec == NotStop )
                      true
                    else
                      false

      }


      val stopEvents = collection.mutable.Map[StopEvent,Double]().withDefaultValue( Double.NegativeInfinity )
      (0 to (s.length-1) ).foreach{ i =>

        ( (i+1) to ( s.length ) ).foreach{ j =>
          val curSpan = Span( i, j )

          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            val stoppage = StopEvent(
              h.obs,
              h.attachmentDirection,
              adj( h, curSpan ),
              Stop
            )

            stopEvents( stoppage ) = logSum(
              stopEvents( stoppage ),
              matrix(i)(j)(h).iScore +
                g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ) , Stop ) +
                  matrix(i)(j)(h.seal.get).oScore -
                    treeScore
            )
          }

          ( (i+1) to (j-1) ).foreach{ k =>

            val rightArguments = matrix(k)(j).keySet.filter{ _.mark == Sealed }
            matrix(i)(k).keySet.filter{ _.attachmentDirection == RightAttachment }.foreach{ h =>
              rightArguments.foreach{ a =>
                val stoppage = StopEvent(
                  h.obs,
                  RightAttachment,
                  adj( h, curSpan ),
                  NotStop
                )

                stopEvents( stoppage ) = logSum(
                  stopEvents( stoppage ),
                  g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                      matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
              }

            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                val stoppage = StopEvent(
                  h.obs,
                  LeftAttachment,
                  adj( h, curSpan ),
                  NotStop
                )

                stopEvents( stoppage ) = logSum(
                  stopEvents( stoppage ),
                  g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                    g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore -
                        treeScore
                )
              }
            }

          }
        }
      }

      stopEvents.keys.toList.sortWith{ _ < _ }.map{ case StopEvent( h, dir, adj, dec ) =>
        List( h.w, h.t, dir, adj, dec, stopEvents( StopEvent( h, dir, adj, dec ) ) )
      }
    }

    def computeMarginals {
      (0 to ((s.length)-1) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          matrix(i)(j).values.foreach( _.computeMarginal )
        }
      }
    }

    def size = s.size

    override def toString = {
      matrix.map{ row =>
        row.map{ x =>
          if( x != null )
            x.values.mkString( "\n", "\n", "\n" )
        }.mkString("\n","\n","\n")
      }.mkString("\n","\n","\n\n")
    }
  }

  class FoldUnfoldChart( s:List[TimedObservedLabel] ) {

    case class SpannedChildren( head:Entry, dependent:Option[Entry] )

    abstract class FoldUnfoldLabel

    abstract class SingleLabel extends FoldUnfoldLabel {
      val obs:TimedObservedLabel
      val v:Boolean
      // def subsequentHead[H<:SingleLabel]:H
    }
    case class LeftwardLabel( obs:TimedObservedLabel, v:Boolean ) extends SingleLabel {
      override def toString = "H_"+obs+".<"+"."+v
      def subsequentHead = LeftwardLabel( obs, false )
    }
    case class RightwardLabel( obs:TimedObservedLabel, v:Boolean ) extends SingleLabel {
      override def toString = "H_"+obs+".>"+"."+v
      def subsequentHead = RightwardLabel( obs, false )
    }

    // This is the "M" label of Johnson (2007)
    // case class SplitLabel( leftObs:LeftwardLabel, rightObs:RightwardLabel ) extends FoldUnfoldLabel {
    // abstract class SplitLabel extends FoldUnfoldLabel
    case class SplitLabel( leftLabel:RightwardLabel, rightLabel:LeftwardLabel )
      extends FoldUnfoldLabel {
      override def toString = leftLabel+".M."+rightLabel
    }
    /*
    case class LeftwardSplit( leftObs:TimedObservedLabel, rightLabel:LeftwardLabel )
      extends SplitLabel {
      override def toString = leftObs+".M."+rightLabel
    }
    */

    case class RootLabel( leftLabel:LeftwardLabel, rightLabel:RightwardLabel ) extends FoldUnfoldLabel {
      override def toString = leftLabel.obs+".R."+rightLabel.obs
    }

    case object StartLabel extends FoldUnfoldLabel {
      override def toString = "S"
    }


    protected val matrix = Array.fill( (2*s.length)+1, (2*s.length)+1 )(
      collection.mutable.Map[FoldUnfoldLabel,Entry]()
    )

    def addEntry( newEntry:Entry ) {
          // println( "Inserting " + newEntry.label + " to " + newEntry.span + " with score " +
          // newEntry.iScore )
      val Span( from, to ) = newEntry.span
      matrix( from ) ( to ) += newEntry.label -> newEntry
    }
    addEntry( StartEntry )

    def apply( start:Int, end:Int ) = matrix(start)(end)

    abstract class Entry( val label:FoldUnfoldLabel, val span:Span ) {

      var iScore = Double.NegativeInfinity
      var oScore = Double.NegativeInfinity
      var score = Double.NegativeInfinity
      def computeMarginal { score = oScore + iScore - treeScore }

      def incrementIScore( inc:Double ) { iScore = logSum( iScore, inc ) }
      def incrementOScore( inc:Double ) {
          // println( "    inc is " + inc  + "; " + exp(inc) )
          // println( "    before " + oScore + " after " + logSum( oScore, inc ) )
        oScore = logSum( oScore, inc )
          // println( "    actually is " + oScore )
      }

      def setOScore( x:Double ) { oScore = x }

      var children = Set[SpannedChildren]()

      def addDependency( headEntry:Entry, argEntry:Entry ):Unit



      override def toString = 
        span + ": " + label +
          "\n  iScore: " + math.exp( iScore ) +
          "\n  oScore: " + math.exp( oScore ) +
          "\n  score: " +  math.exp( score  ) 
    }

    // This is the entry for the "M" label of Johnson (2007) and the "Y" label of Headden et al (2009)
    class SplitEntry( val yNode:SplitLabel, span:Span ) extends Entry( yNode, span ) {

      // val leftObs = yNode.leftLabel.obs
      // val leftV = yNode.leftLabel.v
      // val rightObs = yNode.rightLabel.obs
      // val rightV = yNode.rightLabel.v

      def addDependency( headEntry:Entry, argEntry:Entry ) = {}

      // iScore = logSum(
      //   g.stopScore( StopOrNot( leftObs.w, RightAttachment, leftV ) , Stop ) +
      //     matrix(span.start)(yNode.leftLabel.span.end)( yNode.leftLabel ).iScore ,
      //   g.stopScore( StopOrNot( rightObs.w, LeftAttachment, rightV ) , Stop ) +
      //     matrix(rightObs.t)(span.end)( yNode.rightLabel ).iScore
      //   
      // )

    }

    object StartEntry extends Entry( StartLabel, Span( 0, 2*s.length ) ) {
      oScore = 0D
      def addDependency( headEntry:Entry, argEntry:Entry ) = {}
    }

    class RootEntry( val root:RootLabel, span:Span ) extends Entry( root, span ) {
      assert( root.leftLabel.obs == root.rightLabel.obs )
      val t = root.leftLabel.obs.t
      val r = root.leftLabel.obs.w


      iScore =
        g.stopScore( StopOrNot( r, LeftAttachment, t == 0 ), Stop ) +
        g.stopScore( StopOrNot( r, RightAttachment, t == (s.length-1) ), Stop )  +
            matrix( 0 )( (2*t)+1 )( root.leftLabel ).iScore +
              matrix( (2*t)+1 )( 2*(s.length) )( root.rightLabel ).iScore

      def addDependency( headEntry:Entry, argEntry:Entry ) = {}

    }


    class LeftwardEntry( h:SingleLabel, span:Span ) extends Entry( h, span ) {
      def addDependency( headEntry:Entry, argEntry:Entry ) = {
        val SplitLabel( aLabel, hLabel ) = headEntry.label
        assert( h.obs == hLabel.obs )

        val LeftwardLabel( argEntryObs, argEntryV ) = argEntry.label
        assert( argEntryObs == aLabel.obs )

        assert( argEntryV == ( (2*argEntryObs.t) == argEntry.span.start ) )


        val inc = 
          g.stopScore( StopOrNot( aLabel.obs.w, RightAttachment, aLabel.v  ) , Stop ) +
          g.stopScore( StopOrNot( aLabel.obs.w, LeftAttachment, argEntryV ) , Stop ) +
            g.stopScore( StopOrNot( hLabel.obs.w, LeftAttachment, hLabel.v ) , NotStop ) +
            g.chooseScore( ChooseArgument( hLabel.obs.w, LeftAttachment ) , aLabel.obs.w ) +
              argEntry.iScore + headEntry.iScore

        incrementIScore( inc )

        children += SpannedChildren( headEntry, Some(argEntry) )
      }
    }
    class LeftwardLexEntry( h:SingleLabel, span:Span ) extends LeftwardEntry( h, span ) {
      iScore = 0D
    }

    class RightwardEntry( h:SingleLabel, span:Span ) extends Entry( h, span ) {
      def addDependency( headEntry:Entry, argEntry:Entry ) = {
        val SplitLabel( hLabel, aLabel ) = headEntry.label
        assert( h.obs == hLabel.obs )

          // println( "Attaching " + argEntry.label + " to " + hLabel )

        val RightwardLabel( argEntryObs, argEntryV ) = argEntry.label
        assert( argEntryV == ( 2*(argEntryObs.t+1) == argEntry.span.end ) )


        val inc = 
          g.stopScore( StopOrNot( aLabel.obs.w, LeftAttachment, aLabel.v ) , Stop ) +
          g.stopScore( StopOrNot( aLabel.obs.w, RightAttachment, argEntryV ) , Stop ) +
            g.stopScore( StopOrNot( hLabel.obs.w, RightAttachment, hLabel.v ) , NotStop ) +
            g.chooseScore( ChooseArgument( hLabel.obs.w, RightAttachment ) , aLabel.obs.w ) +
              argEntry.iScore + headEntry.iScore

        incrementIScore( inc )

        children += SpannedChildren( headEntry, Some(argEntry) )
      }
    }
    class RightwardLexEntry( h:SingleLabel, span:Span ) extends RightwardEntry( h, span ) {
      iScore = 0D
    }


    // TODO figure out what to do with root now...
    // def rootEntry = matrix( 0 )( s.length )( rootLabel )
    def treeScore = { matrix( 0 )( 2*s.length )( StartLabel ).iScore }


    // Re-wrote lexFill so we don't explicitly touch iScore at all, only add entries.
    def lexFill( index:Int ) {

        // these are lexical entries so we are guaranteed to have no deps yet, hence v = true
      if( index%2 == 0 ) {
        val w = s(index/2)
        val leftward = LeftwardLabel( w, true )
        addEntry( new LeftwardLexEntry( leftward, Span( index, index+1 ) ) )
      } else {
        val w = s((index-1)/2)
        val rightward = RightwardLabel( w, true )
        addEntry( new RightwardLexEntry( rightward, Span( index, index+1 ) ) )
      }
    }

    def synFill( i:Int, j:Int ) {
      //println( "synFill for " + (i, j) )


      ( (i+1) to (j-1) ).foreach{ k =>

        // println( (i,k,j) + " " + (i%2==0, k%2==0, j%2==0) )

        (i%2==0, k%2==0, j%2==0) match {
          case (true, false, true) => {
            // matrix(i)(k): leftward
            // matrix(k)(j): rightward
            // action: Add root if i == 0 and j == 2*s.length
            if( i == 0 && j == (2*s.length) ) {
              val t = (k-1)/2
              val left = LeftwardLabel( s(t), t == 0 )
              val right = RightwardLabel( s(t), (t+1) == s.length )
              val rootLabel = RootLabel( left, right )

              addEntry( new RootEntry( rootLabel, Span( 0, 2*s.length ) ) )

              matrix( 0 )( 2*s.length )( StartLabel ).incrementIScore(
                g.chooseScore( ChooseArgument( Root, LeftAttachment ), s(t).w ) +
                  matrix( 0 )( (2*s.length) )( rootLabel ).iScore
              )
            }

          }
          case (false, true, false) => {
            // matrix(i)(k): rightward
            // matrix(k)(j): leftward
            // action: add M node

            val rightwardHeads = matrix(i)(k).keySet
            val leftwardHeads = matrix(k)(j).keySet
            assert(
              rightwardHeads.forall{ _ match { case RightwardLabel(_,_) => true ; case _ => false } }
            )
              // println( leftwardHeads.mkString( "{ ", ", ", " }" ) )
            assert(
              leftwardHeads.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
            )

            (
              for {left <- rightwardHeads ; right <- leftwardHeads } yield (left, right )
            ).foreach{ case( left:RightwardLabel, right:LeftwardLabel ) =>
              val split = SplitLabel( left, right )

              if( ! matrix( i )( j ).keySet.contains( split ) )
                addEntry( new SplitEntry( split, Span( i, j ) ) )

              // assert( left.v == ( (left.obs.t+1) == k ) )
              // assert( right.v == ( right.obs.t == k ) )

              // increment split category iScore for stops on the left and right
              matrix( i )( j )( split ).incrementIScore(
                  matrix( i )( k )( left ).iScore + matrix( k )( j )( right ).iScore
              )
            }
          }
          case (false, false, true) => {
            // matrix(i)(k): M node
            // matrix(k)(j): rightward
            // action: rightward attachment
            val splits = matrix(i)(k).keySet
              /// println( splits.mkString( "< ", ", ", " >" ) )
            assert(
              splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
            )
            splits.foreach{ split =>
              val SplitLabel( leftHead, rightHead ) = split

              val leftParent = RightwardLabel( leftHead.obs, false )
              val rightArg = RightwardLabel( rightHead.obs, ( 2*(rightHead.obs.t+1) ) == j )

              if( ! matrix( i )( j ).keySet.contains( leftParent ) )
                addEntry( new RightwardEntry( leftParent, Span( i, j ) ) )
              matrix( i )( j )( leftParent ).addDependency(
                matrix( i )( k )( split ),
                matrix( k )( j )( rightArg )
              )
            }
          }
          case (true, false, false) => {
            // matrix(i)(k): leftward
            // matrix(k)(j): M node
            // action: leftward attachment
            val splits = matrix(k)(j).keySet
            assert(
              splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
            )
            splits.foreach{ split =>
              val SplitLabel( leftHead, rightHead ) = split

              val rightParent = LeftwardLabel( rightHead.obs, false )
              val leftArg = LeftwardLabel( leftHead.obs, (2*leftHead.obs.t) == i )

              if( ! matrix( i )( j ).keySet.contains( rightParent ) )
                addEntry( new LeftwardEntry( rightParent, Span( i, j ) ) )
              matrix( i )( j )( rightParent ).addDependency(
                matrix( k )( j )( split ),
                matrix( i )( k )( leftArg )
              )
            }
          }
          case (true, true, true) => {
            // matrix(i)(k): nothing
            // matrix(k)(j): nothing
            // action: nothing
          }
          case (true, true, false) => {
            // matrix(i)(k): nothing
            // matrix(k)(j): leftward
            // action: nothing
          }
          case (false, true, true) => {
            // matrix(i)(k): rightward
            // matrix(k)(j): nothing
            // action: nothing
          }
          case (false, false, false) => {
            // matrix(i)(k): M node
            // matrix(k)(j): M node
            // action: nothing
          }
        }


      }

    }

    // TODO: modify this for fold-unfold transformed version.
    def outsidePass {
      import math.exp
      val n = 2*s.length

        // println( toString )

      // Each outside score summand is a product of:
      //  - the probability of the rule generating that node as a child
      //  - the inside score of the node's sibling (according to the rule)
      //  - the outside score of the node's parent (according to the rule)

      // Root first
        // println( "Special case S -> L_w R_w  rules" )
      matrix(0)(n).values.filter{
        _ match { case e:RootEntry => true; case _ => false }
      }.foreach{ e =>
        val RootLabel( left, right ) = e.label
        e.incrementOScore(
          g.chooseScore( ChooseArgument( Root, LeftAttachment ), left.obs.w ) +
            matrix(0)(n)(StartLabel).oScore
        )
      }



      // rootEntry.setOScore( 0D )
      ( 1 to n ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>
          val j = i + length
          val curSpan = Span(i,j)

          ( i%2 == 0, j%2 == 0 ) match {
            case (true,true) => {
              // Label type: Root
              if( i == 0 && j == n ) {
                ( 0 to (s.length-1)).foreach{ t =>
                  val k = (2*t)+1
                  val left = LeftwardLabel( s(t), t == 0 )
                  val right = RightwardLabel( s(t), t == (s.length-1) )
                  val rootLabel = RootLabel( left, right )

                  matrix(0)(k)(left).incrementOScore(
                    g.stopScore( StopOrNot( left.obs.w, LeftAttachment, k == 1 ), Stop ) +
                      g.stopScore( StopOrNot( left.obs.w, RightAttachment, k == (n-1) ), Stop ) +
                        matrix(0)(n)(rootLabel).oScore + matrix( k )( n )( right ).iScore
                  )

                  matrix(k)(n)( right ).incrementOScore(
                    g.stopScore( StopOrNot( left.obs.w, LeftAttachment, k == 1 ), Stop ) +
                      g.stopScore( StopOrNot( left.obs.w, RightAttachment, k == (n-1) ), Stop ) +
                        matrix(0)(n)(rootLabel).oScore + matrix( 0 )( k )( left ).iScore
                  )

                }
              }
            }
            case (true,false) => {
              // Label type: LeftwardLabel
              // Two types of parent for LeftwardLabel: M node, and LeftwardLabel

              // Let's do M node first. 
              ( 0 to ((i/2)-1) ).foreach{ leftBoundary =>
                val k = (2*leftBoundary) + 1
                assert( k < i )
                  // println( "  " + (k,i,j) + " M parent" )
                val leftwardHeads = matrix(i)(j).keySet
                val rightwardHeads = matrix(k)(i).keySet
                assert(
                  rightwardHeads.forall{ _ match { case RightwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! rightwardHeads.isEmpty
                )
                assert(
                  leftwardHeads.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! leftwardHeads.isEmpty
                )

                // var splits = matrix(k)(j).keySet
                (
                  for {left <- rightwardHeads ; right <- leftwardHeads } yield (left, right )
                ).foreach{ case( left:RightwardLabel, right:LeftwardLabel ) =>
                // splits.foreach{ case SplitLabel( left, right ) =>
                  val split = SplitLabel( left, right )

                  matrix(i)(j)(right).incrementOScore(
                    matrix(k)(j)(split).oScore + matrix(k)(i)(left).iScore
                  )
                  // splits -= split
                }
                // assert(
                //   splits.isEmpty ||
                //   splits.forall{ case SplitLabel( _, right ) => right.obs != s( (j-1)/2) ; }
                // )
              }


              // Hokay, now let's do LeftwardLabel parent.
              (((j+3)/2) to (s.length) ).foreach{ headBoundary =>
                val k = (2*headBoundary) - 1
                  // println( "  " + (i,j,k) + " Leftward parent")
                assert( k > j )
                val leftArgs = matrix(i)(j).keySet
                val splits = matrix(j)(k).keySet

                assert(
                  leftArgs.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  !( leftArgs.isEmpty )
                )
                assert(
                  splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  !( splits.isEmpty )
                )

                (
                  for { leftArg <- leftArgs ; split <- splits } yield (leftArg, split )
                ).foreach{ case( leftArg:LeftwardLabel, split:SplitLabel ) =>

                  val SplitLabel( leftHead, rightHead ) = split

                  val rightParent = LeftwardLabel( rightHead.obs, false )

                  assert( leftArg.obs == leftHead.obs )

                      // println(
                      //   "P( " + Stop  + " | " + 
                      //     StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) +
                      //   " ) + P( " +
                      //     Stop + " | " + StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) +
                      //   " ) + P( " + NotStop + " | " + StopOrNot( rightHead.obs.w, LeftAttachment,
                      //     rightHead.v ) +
                      //   ") + P( " + leftHead.obs.w + " | " + ChooseArgument( rightHead.obs.w,
                      //     LeftAttachment ) + ") + " +
                      //   (i,k,rightParent) + ".iScore + " +
                      //   (j,k,split) + ".iScore = " +
                      //   g.stopScore( StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) , Stop ) + " + " +
                      //   g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , Stop ) + " + " +
                      //     g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , NotStop ) + " + " +
                      //       g.chooseScore( ChooseArgument( rightHead.obs.w, LeftAttachment ) , leftHead.obs.w ) + " + " +
                      //       matrix( i )( k )( rightParent ).oScore + " + " + matrix( j )( k )( split
                      //       ).iScore +
                      //   " = " + ( g.stopScore( StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) , Stop ) +
                      //   g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , Stop ) +
                      //     g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , NotStop ) +
                      //       g.chooseScore( ChooseArgument( rightHead.obs.w, LeftAttachment ) , leftHead.obs.w ) +
                      //       matrix( i )( k )( rightParent ).oScore + matrix( j )( k )( split ).iScore
                      //   )
                      // )
                  matrix( i )( j )( leftArg ).incrementOScore(
                    g.stopScore( StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) , Stop ) +
                    g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , Stop ) +
                      g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , NotStop ) +
                        g.chooseScore( ChooseArgument( rightHead.obs.w, LeftAttachment ) , leftHead.obs.w ) +
                        matrix( i )( k )( rightParent ).oScore + matrix( j )( k )( split ).iScore
                  )
                }

              }
            }
            case (false,true) => {
              // Label type: RightwardLabel

              // Two types of parent for RightwardLabel: M node, and RightwardLabel

              // Let's do M node first. 
              ( ((j/2)+1) to (s.length) ).foreach{ rightBoundary =>
                val k = (2*rightBoundary)-1
                  // println( "  " + (i,j,k) + " M parent" )
                assert( k > j )
                val rightwardHeads = matrix(i)(j).keySet
                val leftwardHeads = matrix(j)(k).keySet
                assert(
                  rightwardHeads.forall{ _ match { case RightwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! rightwardHeads.isEmpty
                )
                assert(
                  leftwardHeads.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! leftwardHeads.isEmpty
                )

                // var splits = matrix(i)(k).keySet
                // println( splits.mkString( ">>>[ ", ", ", " ]>>>" ) )
                (
                  for {left <- rightwardHeads ; right <- leftwardHeads } yield (left, right )
                ).foreach{ case( left:RightwardLabel, right:LeftwardLabel ) =>
                // splits.foreach{ case SplitLabel( left, right ) =>
                  val split = SplitLabel( left, right )

                  // println( "      } " + (left,right) + " { " )

                  matrix(i)(j)(left).incrementOScore(
                    matrix(i)(k)(split).oScore + matrix(j)(k)(right).iScore
                  )
                  // splits -= split
                }
                // println( splits.mkString( "===[ ", ", ", " ]===" ) )
                // assert(
                //   splits.isEmpty ||
                //   splits.forall{ case SplitLabel( left, _ ) => left.obs != s( (i-1)/2) ; }
                // )
              }


              // Hokay, now let's do RightwardLabel parent.
              (0 to ((i-3)/2)).foreach{ headBoundary =>
                val k = (2*headBoundary) + 1
                  // println( "  " + (k,i,j) + " Rightward parent" )
                assert( k < i )
                val splits = matrix(k)(i).keySet
                val rightArgs = matrix(i)(j).keySet

                assert(
                  splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! splits.isEmpty
                )
                assert(
                  ! rightArgs.isEmpty
                )

                (
                  for {split <- splits ; rightArg <- rightArgs } yield (split, rightArg )
                ).foreach{ case( split:SplitLabel, rightArg:RightwardLabel ) =>

                  val SplitLabel( leftHead, rightHead ) = split

                  val leftParent = RightwardLabel( leftHead.obs, false )

                  assert( rightArg.obs == rightHead.obs )

                  matrix( i )( j )( rightArg ).incrementOScore(
                    g.stopScore( StopOrNot( rightHead.obs.w, RightAttachment, rightArg.v ) , Stop ) +
                    g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , Stop ) +
                      g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , NotStop ) +
                        g.chooseScore( ChooseArgument( leftHead.obs.w, RightAttachment ) , rightHead.obs.w ) +
                        matrix( k )( j )( leftParent ).oScore + matrix( k )( i )( split ).iScore
                  )
                }

              }
            }
            case (false,false) => {
              // Label type: SplitLabel
              (0 to ((i-1)/2)).foreach{ argBoundary =>
                val k = 2*argBoundary
                  // println( "  " + (k,i,j) + " Leftward parent" )
                assert( k < i )
                val splits = matrix(i)(j).keySet
                val leftArgs = matrix(k)(i).keySet
                assert(
                  leftArgs.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! splits.isEmpty
                )
                assert(
                  ! leftArgs.isEmpty
                )

                (
                  for {leftArg <- leftArgs ; split <- splits } yield (leftArg, split)
                ).foreach{ case( leftArg:LeftwardLabel, split:SplitLabel ) =>
                  val SplitLabel( leftHead, rightHead ) = split
                  val rightParent = LeftwardLabel( rightHead.obs, false )

                    // println( "      split is " + split )
                    // println( "      " + rightHead + " --> " + (leftArg,leftHead) )

                  assert( leftArg.obs == leftHead.obs )
                  if( ( k == 2 * leftArg.obs.t ) )
                    assert( leftArg.v == true )
                  if( (j-1) == 2* leftHead.obs.t )
                    assert( leftHead.v == true )
                  if( (i+1) == 2* rightHead.obs.t )
                    assert( rightHead.v == true )


                  matrix( i )( j )( split ).incrementOScore(
                    g.stopScore( StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) , Stop ) +
                    g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , Stop ) +
                      g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , NotStop ) +
                        g.chooseScore( ChooseArgument( rightHead.obs.w, LeftAttachment ) , leftHead.obs.w ) +
                          matrix( k )( j )( rightParent ).oScore + matrix( k )( i )( leftArg ).iScore
                  )
                }

              }
              (((j+1)/2) to (s.length)).foreach{ argBoundary =>
                val k = 2*(argBoundary)
                assert( k > j )
                  // println( "  " + (i,j,k) + " Rightward parent")
                val splits = matrix(i)(j).keySet
                val rightArgs = matrix(j)(k).keySet
                assert(
                  rightArgs.forall{ _ match { case RightwardLabel(_,_) => true ; case _ => false } }
                )
                assert(
                  ! splits.isEmpty
                )
                assert(
                  ! rightArgs.isEmpty
                )
                (
                  for {split <- splits ; rightArg <- rightArgs } yield (split, rightArg )
                ).foreach{ case( split:SplitLabel, rightArg:RightwardLabel ) =>
                  val SplitLabel( leftHead, rightHead ) = split

                  val leftParent = RightwardLabel( leftHead.obs, false )

                    // println( "      split is " + split )
                    // println( "      " + leftHead + " --> " + (rightHead,rightArg) )

                  assert( rightArg.obs == rightHead.obs )
                  if( k == 2*( rightArg.obs.t+1 ) )
                    assert( rightArg.v == true )
                  if( (j-1) == 2* leftHead.obs.t )
                    assert( leftHead.v == true )
                  if( (i+1) == 2* rightHead.obs.t )
                    assert( rightHead.v == true )

                  matrix( i )( j )( split ).incrementOScore(
                    g.stopScore( StopOrNot( rightHead.obs.w, RightAttachment, rightArg.v ) , Stop ) +
                    g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , Stop ) +
                      g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , NotStop ) +
                        g.chooseScore( ChooseArgument( leftHead.obs.w, RightAttachment ) , rightHead.obs.w ) +
                        matrix( i )( k )( leftParent ).oScore + matrix( j )( k )( rightArg ).iScore
                  )
                }


              }
            }
          }


        }
      )
    }


    // TODO: modify this for fold-unfold transformed version.
    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = g.emptyPartialCounts

      val n = 2*s.length


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

      matrix(0)(n).values.filter{
        _ match { case e:RootEntry => true; case _ => false }
      }.foreach{ e =>
        val RootLabel( left, right ) = e.label
        pc.incrementChooseCounts(
          ChooseArgument( Root, LeftAttachment ),
          left.obs.w,
          e.iScore +
            g.chooseScore( ChooseArgument( Root, LeftAttachment ), left.obs.w ) - treeScore
        )

        val k = (2*left.obs.t) + 1
        val rootStoppingScore =
          g.stopScore( StopOrNot( left.obs.w, LeftAttachment, left.v ), Stop ) +
            g.stopScore( StopOrNot( left.obs.w, RightAttachment, right.v ), Stop ) +
              matrix(0)(k)(left).iScore + matrix(k)(n)(right).iScore + e.oScore - treeScore

        pc.incrementStopCounts(
          StopOrNot( left.obs.w, LeftAttachment, left.v ),
          Stop,
          rootStoppingScore
        )
        pc.incrementStopCounts(
          StopOrNot( left.obs.w, RightAttachment, right.v ),
          Stop,
          rootStoppingScore
        )
      }


      (0 to (n-1) ).foreach{ i =>

        ( (i+1) to n ).foreach{ j =>
          val curSpan = Span( i, j )
          ( (i+1) to (j-1) ).foreach{ k =>

            (i%2==0, k%2==0, j%2==0) match {
              case (false, false, true) => {
                // matrix(i)(k): M node
                // matrix(k)(j): rightward
                // attachment type: rightward
                val splits = matrix(i)(k).keySet
                splits.foreach{ split =>
                  val SplitLabel( leftHead, rightHead ) = split

                  val leftParent = RightwardLabel( leftHead.obs, false )
                  val rightArg = RightwardLabel( rightHead.obs, ( 2*(rightHead.obs.t+1) ) == j )

                  val attScore =
                    g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , Stop ) +
                    g.stopScore( StopOrNot( rightHead.obs.w, RightAttachment, rightArg.v ) , Stop ) +
                      g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , NotStop ) +
                      g.chooseScore( ChooseArgument( leftHead.obs.w, RightAttachment ) , rightHead.obs.w ) +
                        matrix(i)(k)(split).iScore + matrix(k)(j)(rightArg).iScore +
                        matrix(i)(j)(leftParent).oScore - treeScore
                        // argEntry.iScore + headEntry.iScore

                  pc.incrementStopCounts(
                    StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ),
                    Stop,
                    attScore
                  )
                  pc.incrementStopCounts(
                    StopOrNot( rightHead.obs.w, RightAttachment, rightArg.v ),
                    Stop,
                    attScore
                  )
                  pc.incrementStopCounts(
                    StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ),
                    NotStop,
                    attScore
                  )
                  pc.incrementChooseCounts(
                    ChooseArgument( leftHead.obs.w, RightAttachment ),
                    rightHead.obs.w,
                    attScore
                  )
                }
              }
              case (true, false, false) => {
                // matrix(i)(k): leftward
                // matrix(k)(j): M node
                // action: leftward attachment
                val splits = matrix(k)(j).keySet
                splits.foreach{ split =>
                  val SplitLabel( leftHead, rightHead ) = split

                  val rightParent = LeftwardLabel( rightHead.obs, false )
                  val leftArg = LeftwardLabel( leftHead.obs, (2*leftHead.obs.t) == i )

                  val attScore =
                    g.stopScore( StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ) , Stop ) +
                    g.stopScore( StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ) , Stop ) +
                      g.stopScore( StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ) , NotStop ) +
                      g.chooseScore( ChooseArgument( rightHead.obs.w, LeftAttachment ) , leftHead.obs.w ) +
                        matrix(k)(j)(split).iScore + matrix(i)(k)(leftArg).iScore +
                        matrix(i)(j)(rightParent).oScore - treeScore
                        // argEntry.iScore + headEntry.iScore

                  pc.incrementStopCounts(
                    StopOrNot( leftHead.obs.w, RightAttachment, leftHead.v ),
                    Stop,
                    attScore
                  )
                  pc.incrementStopCounts(
                    StopOrNot( leftHead.obs.w, LeftAttachment, leftArg.v ),
                    Stop,
                    attScore
                  )
                  pc.incrementStopCounts(
                    StopOrNot( rightHead.obs.w, LeftAttachment, rightHead.v ),
                    NotStop,
                    attScore
                  )
                  pc.incrementChooseCounts(
                    ChooseArgument( rightHead.obs.w, LeftAttachment ),
                    leftHead.obs.w,
                    attScore
                  )
                }
              }
              case (false, true, false) => {
              }
              case (true, false, true) => {
              }
              case (true, true, true) => {
              }
              case (true, true, false) => {
              }
              case (false, true, true) => {
              }
              case (false, false, false) => {
              }
            }















          }

        }
      }

      if( !( treeScore <= 0D ) ) {
        println( s.mkString("[ ", " ", " ]" ) )
        println( treeScore )
        println( this )
      }
      assert( treeScore <= 0D )
      pc.setTotalScore( treeScore )

      pc
    }

    def computeMarginals {
      (0 to ((2*s.length)-1) ).foreach{ i =>
        ( (i+1) to (2*s.length) ).foreach{ j =>
          matrix(i)(j).values.foreach( _.computeMarginal )
        }
      }
    }

    def size = 2*s.size

    override def toString = {
      matrix.map{ row =>
        row.map{ x =>
          if( x != null )
            x.values.mkString( "\n", "\n", "\n" )
        }.mkString("\n","\n","\n")
      }.mkString("\n","\n","\n\n")
    }
  }

  /*
  * This is the CYK parsing algorithm.
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with inside and outside probabilities.
  */
  def populateChart( s:List[TimedObservedLabel] ) = {
    val chart =
      if( s.last != FinalRoot( s.length ) )
        new Chart( s :+ FinalRoot( s.length ) )
      else
        new Chart( s )

    (1 to ( chart.size )) foreach{ j =>
      chart.lexFill( j-1 )
      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          chart.synFill( i , j )
        }
    }

    chart.outsidePass

    chart.computeMarginals

    chart
  }

  /*
  * This is the CYK parsing algorithm with the Fold-Unfold transform (Johnson, 2007)
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with inside and outside probabilities.
  */
  def efficientPopulateChart( s:List[TimedObservedLabel] ) = {
    val chart = new FoldUnfoldChart( s )

    (1 to ( chart.size )) foreach{ j =>
      chart.lexFill( j-1 )
      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          chart.synFill( i , j )
        }
    }

    chart.outsidePass

    // chart.computeMarginals

    chart
  }

  def maxMarginalParse( corpus:List[AbstractTimedSentence], prefix:String ) =
    corpus/*.par*/.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":dependency:" + id + " " + chart.toMaxMarginalDependencyParse + "\n" +
          prefix + ":constituency:" + id + " " + chart.toMaxMarginalConstituencyParse
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":dependency:" + id + " " + chart.toMaxMarginalDependencyParse + "\n" +
          prefix + ":constituency:" + id + " " + chart.toMaxMarginalConstituencyParse
        }
      }
    }

  def annotatedMaxMarginalParse( corpus:List[AbstractTimedSentence], prefix:String ) =
    corpus.par.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":constituency:" + id + " " + chart.toAnnotatedMaxMarginalConstituencyParse
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":constituency:" + id + " " + chart.toAnnotatedMaxMarginalConstituencyParse
        }
      }
    }
    // corpus.par.map{ s =>
    //   val pc = populateChart(s).toPartialCounts
    //   pc
    // }.reduceLeft{(a,b) =>
    //   a.destructivePlus(b);
    //   a
    // }

  def arcProbabilitiesCSV( corpus:List[AbstractTimedSentence] ) =
    corpus/*.par*/.map{ _ match {
        case TimedSentence( id, s ) => {
          populateChart( s ).arcProbabilities.map{ arcs =>
            "arcProbabilities," + id + "," + arcs.mkString("",",","")
          }.mkString("","\n","")
        }
        case TimedTwoStreamSentence( id, s ) => {
          populateChart( s ).arcProbabilities.map{ arcs =>
            "arcProbabilities," + id + "," + arcs.mkString("",",","")
          }.mkString("","\n","")
        }
      }
    }

  def stopProbabilitiesCSV( corpus:List[AbstractTimedSentence] ) =
    corpus/*.par*/.map{ _ match {
        case TimedSentence( id, s ) => {
          populateChart( s ).stopProbabilities.map{ stoppages =>
            "stopProbabilities," + id + "," + stoppages.mkString("",",","")
          }.mkString("","\n","")
        }
        case TimedTwoStreamSentence( id, s ) => {
          populateChart( s ).stopProbabilities.map{ stoppages =>
            "stopProbabilities," + id + "," + stoppages.mkString("",",","")
          }.mkString("","\n","")
        }
      }
    }

  def stopAndArcProbabilitiesCSV( corpus:List[AbstractTimedSentence] ) =
    corpus/*.par*/.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          chart.arcProbabilities.map{ arcs =>
            "arcProbabilities," + id + "," + arcs.mkString("",",","")
          }.mkString("","\n","\n") +
          chart.stopProbabilities.map{ stoppages =>
            "stopProbabilities," + id + "," + stoppages.mkString("",",","")
          }.mkString("","\n","")
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          chart.arcProbabilities.map{ arcs =>
            "arcProbabilities," + id + "," + arcs.mkString("",",","")
          }.mkString("","\n","") +
          chart.stopProbabilities.map{ stoppages =>
            "stopProbabilities," + id + "," + stoppages.mkString("",",","")
          }.mkString("","\n","\n")
        }
      }
    }

  def spanProbabilitiesCSV( corpus:List[AbstractTimedSentence] ) = {
    "spanVarScores,uttID," + List(
      "s",
      "e",
      "h_index",
      "d_index",
      "h_word",
      "d_word",
      "dir",
      "varScore"
      ).mkString("",",","\n") +
    "spanEntropy,uttID," + List(
        "s",
        "e",
        "h_index",
        "h_word",
        "dir",
        "entropy"
      ).mkString("",",","\n")::corpus/*.par*/.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          chart.spanProbabilities.map{ spans =>
            // spans("marginals").map{ spanVarScore =>
            //   "spanVarScores"::id::spanVarScore//.mkString("",",","\n")
            // }.mkString("",",","\n") +
            spans("marginals").mkString("spanVarScores,"+id+",","\nspanVarScores,"+id+",","\n") +
            "spanEntropy," + id + "," + spans("entropy").mkString("",",","\n")
          }.mkString("","\n","\n")
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          chart.spanProbabilities.map{ spans =>
            // spans("marginals").map{ spanVarScore =>
            //   "spanVarScores" + id + "," + spanVarScore//.mkString("",",","\n")
            // }.mkString("",",","\n") +
            spans("marginals").mkString("spanVarScores,"+id+",","\nspanVarScores,"+id+",","\n") +
            "spanEntropy," + id + "," + spans("entropy").mkString("",",","\n")
          }.mkString("","\n","")
        }
      }
    }
  }

  def partialCountsCSV( corpus:List[AbstractTimedSentence] ) =
    corpus.par.map{ _ match {
        case TimedSentence( id, s ) => {
          val pc = populateChart( s ).toPartialCounts
          pc.chooseCSV.map{ csv => "chooseCounts," + id + "," + csv }.mkString("","\n","\n") +
          pc.stopCSV.map{ csv => "stopCounts," + id + "," + csv }.mkString("","\n","")
        }
        case TimedTwoStreamSentence( id, s ) => {
          val pc = populateChart( s ).toPartialCounts
          pc.chooseCSV.map{ csv =>"chooseCounts," + id + "," + csv }.mkString("","\n","\n") +
          pc.stopCSV.map{ csv => "stopCounts," + id + "," + csv }.mkString("","\n","")
          //prefix + ":constituency:" + id + " " + chart.toAnnotatedMaxMarginalConstituencyParse
        }
      }
    }

  def computePartialCounts( corpus:Iterable[List[TimedObservedLabel]] ) =
    corpus.par.map{ s =>
      val pc = populateChart(s).toPartialCounts
      pc
    //}.toList.reduceLeft{(a,b) =>
    }.toList.foldLeft( g.emptyPartialCounts ){(a,b) =>
      a.destructivePlus(b);
      a
    }

  def efficientComputePartialCounts( corpus:Iterable[List[TimedObservedLabel]] ) =
    corpus.par.map{ s =>
      val pc = efficientPopulateChart(s).toPartialCounts
      pc
    //}.toList.reduceLeft{(a,b) =>
    }.toList.foldLeft( g.emptyPartialCounts ){(a,b) =>
      a.destructivePlus(b);
      a
    }


}

class VanillaDMVParser( val randomSeed:Int = 10 ) extends AbstractDMVParser{
  import scala.util.Random
  private val r = new Random(randomSeed)

  val g:AbstractDMVGrammar = new DMVGrammar//( vocabulary = Set[ObservedLabel]() )

  // def setGrammar( givenGrammar:AbstractDMVGrammar ) {
  //   println( "VanillaDMVParser.setGrammar" )
  //   //g.setParams( givenGrammar.getVanillaParams )
  //   g.setParams( givenGrammar.getParams )
  //   println( "viterbi grammar is:\n" + g )
  // }


  class ViterbiChart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,Entry]()
    )

    def apply( start:Int, end:Int ) = matrix(start)(end)


    // Ok, stupidly simple entry class
    abstract class Entry(
      val label:MarkedObservation,
      val span:Span
    ) {
      def constituencyParse:String
      def dependencyParse:Set[DirectedArc]
      var headChild:Option[Entry] = None
      var argChild:Option[Entry] = None
      var score:Double = Double.NegativeInfinity

      def addDependency( headEntry:Entry, argEntry:Entry ) {
        val h = headEntry.label
        assert( label == h )
        assert(
          ( headEntry.span.start == argEntry.span.end && h.attachmentDirection == LeftAttachment ) ||
          ( headEntry.span.end == argEntry.span.start && h.attachmentDirection == RightAttachment )
        )
        val a = argEntry.label

        if( a.obs.w != Root ) {
          val newDependencyScore = 
            g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) +
            g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) +
            argEntry.score +
            headEntry.score

          if(
            argChild.isEmpty ||
            ( newDependencyScore > score ) ||
            ( newDependencyScore == score && r.nextDouble >= 0.5 )
          ) {
            headChild = Some( headEntry )
            argChild = Some( argEntry )
            score = newDependencyScore
          }
        }

      }

      override def toString = 
        span + ": " + label + {
          if ( argChild.isEmpty )
            "\n  score: " +  math.exp( score )
          else
            "\n  --> " + argChild.get.label + ": " +  math.exp( score  )
        }
    }


    class UnaryEntry(
      //label:MarkedObservation,
      headEntry:Entry
    ) extends Entry( headEntry.label.seal.get, headEntry.span ) {
      headChild = Some( headEntry )
      score = g.stopScore(
          StopOrNot(label.obs.w, headEntry.label.attachmentDirection, adj(headEntry.label,span) ),
          Stop
        ) + headEntry.score

      def constituencyParse = "(" + label + " " + headChild.get.constituencyParse + " )"
      def dependencyParse = headChild.get.dependencyParse
    }

    class TerminalEntry( label:MarkedObservation, index:Int )
      extends Entry( label, Span( index, index + 1 ) ) {

      score = label.mark match {
        case UnsealedLeftFirst =>
          g.orderScore( label.obs.w, LeftFirst )
        case UnsealedRightFirst =>
          g.orderScore( label.obs.w, RightFirst )
      }

      def constituencyParse = "(" + label + " " + label.obs + ")"
      def dependencyParse = Set[DirectedArc]()
    }

    class BinaryEntry(
      headEntry:Entry,
      span:Span
    ) extends Entry( headEntry.label, span ) {

      headChild = Some( headEntry )

      def dependencyParse =
        headChild.get.dependencyParse ++
          argChild.get.dependencyParse +
            DirectedArc(label.obs, argChild.get.label.obs )

      def constituencyParse =
        label.mark.attachmentDirection match {
          case RightAttachment =>
            "(" + label + " " +
              headChild.get.constituencyParse + " " + argChild.get.constituencyParse +
            " ) "
          case LeftAttachment =>
            "(" + label + " " +
              argChild.get.constituencyParse + " " + headChild.get.constituencyParse +
            " ) "
        }
    }


    def lexFill( index:Int ) {
      val w = s(index)


      if( g.orderScore( w.w , LeftFirst ) > g.orderScore( w.w , RightFirst ) ) {
        val unsealedLeftFirst = MarkedObservation( w, UnsealedLeftFirst )
        matrix(index)(index+1) +=
          unsealedLeftFirst -> new TerminalEntry( unsealedLeftFirst, index )

        val sealedLeft = unsealedLeftFirst.seal.get
        matrix(index)(index+1) +=
          sealedLeft -> new UnaryEntry( matrix(index)(index+1)(unsealedLeftFirst) )

        val sealedBoth = sealedLeft.seal.get
        matrix(index)(index+1) +=
          sealedBoth -> new UnaryEntry( matrix(index)(index+1)(sealedLeft) )

      } else {
        val unsealedRightFirst = MarkedObservation( w, UnsealedRightFirst )
        matrix(index)(index+1) +=
          unsealedRightFirst -> new TerminalEntry( unsealedRightFirst, index )

        val sealedRight = unsealedRightFirst.seal.get
        matrix(index)(index+1) +=
          sealedRight -> new UnaryEntry( matrix(index)(index+1)(unsealedRightFirst) )

        val sealedBoth = sealedRight.seal.get
        matrix(index)(index+1) +=
          sealedBoth -> new UnaryEntry( matrix(index)(index+1)(sealedRight) )
      }
    }

    // Re-writing synFill so we don't explicitly touch score at all, only add entries and
    // dependents.
    def synFill( start:Int, end:Int ) {
      // First, let's just add all the possible heads with possible dependencies. Since we are
      // guaranteed that end-start>1, we cannot have any seals until we have added heads to the
      // current span.

      ( (start+1) to (end-1) ).foreach{ k =>
        val rightArguments = matrix(k)(end).keySet.filter{ _.mark == Sealed }.filterNot{ _.obs.w == Root }
        val leftArguments = matrix(start)(k).keySet.filter{ _.mark == Sealed }

        val unsealedLeftHeads = matrix(start)(k).keySet.filter{ _.mark == UnsealedRightFirst }
        unsealedLeftHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) )
            matrix(start)(end) += h -> new BinaryEntry( matrix(start)(k)(h), Span(start,end) )

          rightArguments.foreach{ a =>
            matrix(start)(end)(h).addDependency(
              matrix(start)(k)(h),
              matrix(k)(end)(a)
            )
          }
        }

        val unsealedRightHeads = matrix(k)(end).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) )
            matrix(start)(end) += h -> new BinaryEntry( matrix(k)(end)(h), Span(start,end) )

          leftArguments.foreach{ a => 
            matrix(start)(end)(h).addDependency(
              matrix(k)(end)(h),
              matrix(start)(k)(a)
            )
          }
        }



        val halfSealedLeftHeads =
          matrix(start)(k).keySet.filter{ _.mark == SealedLeft }
        halfSealedLeftHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) )
            matrix(start)(end) += h -> new BinaryEntry( matrix(start)(k)(h), Span(start,end) )

          rightArguments.foreach{ a =>
            matrix(start)(end)(h).addDependency(
              matrix(start)(k)(h),
              matrix(k)(end)(a)
            )
          }
        }

        val halfSealedRightHeads =
          matrix(k)(end).keySet.filter{ _.mark == SealedRight }
        halfSealedRightHeads.foreach{ h =>
          if( !( matrix(start)(end).keySet.contains( h ) ) )
            matrix(start)(end) += h -> new BinaryEntry( matrix(k)(end)(h), Span(start,end) )

          leftArguments.foreach{ a =>
            matrix(start)(end)(h).addDependency(
              matrix(k)(end)(h),
              matrix(start)(k)(a)
            )
          }
        }

      }

      matrix(start)(end).keySet.filter{ h =>
        (h.mark == UnsealedLeftFirst ) | (h.mark == UnsealedRightFirst)
      }.foreach{ h =>
        if( !( matrix(start)(end).keySet.contains( h.seal.get ) ) )
          matrix(start)(end) += h.seal.get -> new UnaryEntry( matrix(start)(end)(h) )
      }
      matrix(start)(end).keySet.filter{ h =>
        (h.mark == SealedLeft ) | (h.mark == SealedRight)
      }.foreach{ h =>
        if( !( matrix(start)(end).keySet.contains( h.seal.get ) ) )
          matrix(start)(end) += h.seal.get -> new UnaryEntry( matrix(start)(end)(h) )
      }
    }


    def size = s.size

    def rootEntry = matrix( 0 )( s.length )( MarkedObservation( FinalRoot(s.length-1), Sealed ) )
    def treeScore = rootEntry.score

    def toConstituencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length-1 ) , Sealed ) ).constituencyParse

    def toConstituencyParseWithLogProb = ScoredParse( toConstituencyParse, treeScore )

    def toDependencyParse =
      matrix(0)(s.length)(
        MarkedObservation( FinalRoot( s.length-1 ) , Sealed )
      ).dependencyParse.toList.sortWith{ _.arg.t < _.arg.t }.map{_.head.t}.mkString( "[ ", ", ", " ] " )

    def toDependencyParseWithLogProb = ScoredParse( toDependencyParse, treeScore )

    override def toString =
      matrix.map{ row =>
        row.map{ x =>
          if( x != null )
            x.values.mkString( "\n", "\n", "\n" )
        }.mkString("\n","\n","\n")
      }.mkString("\n","\n","\n\n")
  }

  case class ScoredParse( parse:String, logProb:Double )

        // class FoldUnfoldVitChart( s:List[TimedObservedLabel] ) {

        //   case class SpannedChildren( head:Entry, dependent:Option[Entry] )

        //   abstract class FoldUnfoldLabel

        //   abstract class SingleLabel extends FoldUnfoldLabel {
        //     val obs:TimedObservedLabel
        //     val v:Boolean
        //   }
        //   case class LeftwardLabel( obs:TimedObservedLabel, v:Boolean ) extends SingleLabel {
        //     override def toString = "H_"+obs+".<"+"."+v
        //     def subsequentHead = LeftwardLabel( obs, false )
        //   }
        //   case class RightwardLabel( obs:TimedObservedLabel, v:Boolean ) extends SingleLabel {
        //     override def toString = "H_"+obs+".>"+"."+v
        //     def subsequentHead = RightwardLabel( obs, false )
        //   }

        //   // This is the "M" label of Johnson (2007)
        //   case class SplitLabel( leftLabel:RightwardLabel, rightLabel:LeftwardLabel )
        //     extends FoldUnfoldLabel {
        //     override def toString = leftLabel+".M."+rightLabel
        //   }

        //   case class RootLabel( leftLabel:LeftwardLabel, rightLabel:RightwardLabel ) extends FoldUnfoldLabel {
        //     override def toString = leftLabel.obs+".R."+rightLabel.obs
        //   }

        //   case object StartLabel extends FoldUnfoldLabel {
        //     override def toString = "S"
        //   }


        //   protected val matrix = Array.fill( (2*s.length)+1, (2*s.length)+1 )(
        //     collection.mutable.Map[FoldUnfoldLabel,Entry]()
        //   )

        //   def addEntry( newEntry:Entry ) {
        //     val Span( from, to ) = newEntry.span
        //     matrix( from ) ( to ) += newEntry.label -> newEntry
        //   }
        //   addEntry( StartEntry )

        //   def addRootEntry( newEntry:Entry ) {
        //     assert( newEntry.span == Span( 0, 2 * s.length) )


        //     val RootLabel( left, right ) = newEntry.label
        //     val k = (2*left.obs.t) + 1
        //     newEntry.children =
        //       Some(
        //         SpannedChildren(
        //           matrix(0)(k)(left),
        //           Some( matrix(k)(2*s.length)(right) )
        //         )
        //       )

        //     matrix( 0 ) ( 2*s.length ) += newEntry.label -> newEntry

        //     val newTreeScore =
        //       newEntry.iScore +
        //         g.chooseScore( ChooseArgument( Root, LeftAttachment ), left.obs.w )

        //     if( newTreeScore > treeScore ) {
        //       matrix( 0 )( 2*s.length )( StartLabel ).children =
        //         Some( SpannedChildren( newEntry , None ) )
        //       matrix( 0 )( 2*s.length )( StartLabel ).setIScore( newTreeScore )
        //     }
        //   }

        //   def apply( start:Int, end:Int ) = matrix(start)(end)

        //   def apply( span:Span ) = matrix( span.start )( span.end )

        //   abstract class Entry( val label:FoldUnfoldLabel, val span:Span ) {

        //     var iScore = Double.NegativeInfinity
        //     var oScore = Double.NegativeInfinity
        //     var score = Double.NegativeInfinity
        //     def computeMarginal { score = oScore + iScore - treeScore }

        //     def incrementIScore( inc:Double ) { iScore = logSum( iScore, inc ) }
        //     def incrementOScore( inc:Double ) { oScore = logSum( oScore, inc ) }

        //     def setOScore( x:Double ) { oScore = x }
        //     def setIScore( x:Double ) { iScore = x }

        //     // var children = Set[SpannedChildren]()
        //     var children:Option[SpannedChildren] = None

        //     def addDependency( headEntry:Entry, argEntry:Entry ):Unit

        //     def dependencyParse:Set[DirectedArc]
        //     def constituencyParse:String

        //     override def toString = 
        //       span + ": " + label +
        //         "\n  iScore: " + math.exp( iScore ) +
        //         "\n  oScore: " + math.exp( oScore ) +
        //         "\n  score: " +  math.exp( score  ) 
        //   }

        //   // This is the entry for the "M" label of Johnson (2007) and the "Y" label of Headden et al (2009)
        //   class SplitEntry( val yNode:SplitLabel, span:Span ) extends Entry( yNode, span ) {
        //     def addDependency( left:Entry, right:Entry ) = {
        //       val newIScore = left.iScore + right.iScore
        //       if( newIScore > iScore ) {
        //         setIScore( newIScore )
        //         children = Some( SpannedChildren( left, Some( right ) ) )
        //       }
        //     }

        //     lazy val SplitLabel( leftChild, rightChild ) = yNode
        //     lazy val SpannedChildren( leftEntry, rightOption ) = children.get
        //     lazy val rightEntry = rightOption.get

        //     def dependencyParse =
        //       leftEntry.dependencyParse ++
        //         rightEntry.dependencyParse
        //     def constituencyParse =
        //       "(" + yNode + " " + leftEntry.constituencyParse + " " +
        //       rightEntry.constituencyParse + " ) "
        //   }

        //   object StartEntry extends Entry( StartLabel, Span( 0, 2*s.length ) ) {
        //     oScore = 0D
        //     def addDependency( headEntry:Entry, argEntry:Entry ) = {}

        //     lazy val SpannedChildren( child, _ ) = children.get
        //     def dependencyParse = {
        //       child.dependencyParse
        //     }
        //     def constituencyParse = "(S " + child.constituencyParse + " )"
        //   }

        //   class RootEntry( val root:RootLabel, span:Span ) extends Entry( root, span ) {
        //     assert( root.leftLabel.obs == root.rightLabel.obs )
        //     val t = root.leftLabel.obs.t
        //     val r = root.leftLabel.obs.w


        //     iScore =
        //       g.stopScore( StopOrNot( r, LeftAttachment, t == 0 ), Stop ) +
        //       g.stopScore( StopOrNot( r, RightAttachment, t == (s.length-1) ), Stop )  +
        //           matrix( 0 )( (2*t)+1 )( root.leftLabel ).iScore +
        //             matrix( (2*t)+1 )( 2*(s.length) )( root.rightLabel ).iScore

        //     lazy val SpannedChildren( left, Some(right) ) = children.get
        //     def dependencyParse =
        //       left.dependencyParse ++
        //         right.dependencyParse
        //     def constituencyParse =
        //       "(" + root +
        //         left.constituencyParse +
        //           right.constituencyParse +
        //       " )"

        //     def addDependency( headEntry:Entry, argEntry:Entry ) = {}

        //   }


        //   class LeftwardEntry( h:SingleLabel, span:Span ) extends Entry( h, span ) {

        //     lazy val SpannedChildren( head, argOption ) = children.get
        //     lazy val arg = argOption.get
        //     lazy val LeftwardLabel( argObs, argV ) = arg.label


        //     def dependencyParse =
        //       Set( DirectedArc( h.obs, argObs ) ) ++
        //       head.dependencyParse ++
        //         arg.dependencyParse
        //     def constituencyParse =
        //       "(" + h +
        //         arg.constituencyParse +
        //           head.constituencyParse +
        //       " )"

        //     def addDependency( headEntry:Entry, argEntry:Entry ) = {
        //       val SplitLabel( aLabel, hLabel ) = headEntry.label
        //       assert( h.obs == hLabel.obs )

        //       val LeftwardLabel( argEntryObs, argEntryV ) = argEntry.label
        //       assert( argEntryObs == aLabel.obs )

        //       assert( argEntryV == ( (2*argEntryObs.t) == argEntry.span.start ) )

        //       val newIScore = 
        //         g.stopScore( StopOrNot( aLabel.obs.w, RightAttachment, aLabel.v  ) , Stop ) +
        //         g.stopScore( StopOrNot( aLabel.obs.w, LeftAttachment, argEntryV ) , Stop ) +
        //           g.stopScore( StopOrNot( hLabel.obs.w, LeftAttachment, hLabel.v ) , NotStop ) +
        //           g.chooseScore( ChooseArgument( hLabel.obs.w, LeftAttachment ) , aLabel.obs.w ) +
        //             argEntry.iScore + headEntry.iScore

        //       if( newIScore > iScore ) {
        //         setIScore( newIScore )
        //         children = Some( SpannedChildren( headEntry, Some( argEntry ) ) )
        //       }


        //     }
        //   }
        //   class LeftwardLexEntry( h:SingleLabel, span:Span ) extends LeftwardEntry( h, span ) {
        //     iScore = 0D
        //     children = None

        //     override def dependencyParse = Set[DirectedArc]()
        //     override def constituencyParse = "(" + h + " " + h.obs + " )"
        //   }

        //   class RightwardEntry( h:SingleLabel, span:Span ) extends Entry( h, span ) {

        //     lazy val SpannedChildren( head, argOption ) = children.get
        //     lazy val arg = argOption.get
        //     lazy val RightwardLabel( argObs, argV ) = arg.label

        //     def dependencyParse =
        //       Set( DirectedArc( h.obs, argObs ) ) ++
        //       head.dependencyParse ++
        //         arg.dependencyParse
        //     def constituencyParse =
        //       "(" + h +
        //         head.constituencyParse +
        //           arg.constituencyParse +
        //       " )"


        //     def addDependency( headEntry:Entry, argEntry:Entry ) = {
        //       val SplitLabel( hLabel, aLabel ) = headEntry.label
        //       assert( h.obs == hLabel.obs )

        //         // println( "Attaching " + argEntry.label + " to " + hLabel )

        //       val RightwardLabel( argEntryObs, argEntryV ) = argEntry.label
        //       // assert( argEntryObs == aLabel.obs )
        //       assert( argEntryV == ( 2*(argEntryObs.t+1) == argEntry.span.end ) )


        //       val newIScore = 
        //         g.stopScore( StopOrNot( aLabel.obs.w, LeftAttachment, aLabel.v ) , Stop ) +
        //         g.stopScore( StopOrNot( aLabel.obs.w, RightAttachment, argEntryV ) , Stop ) +
        //           g.stopScore( StopOrNot( hLabel.obs.w, RightAttachment, hLabel.v ) , NotStop ) +
        //           g.chooseScore( ChooseArgument( hLabel.obs.w, RightAttachment ) , aLabel.obs.w ) +
        //             argEntry.iScore + headEntry.iScore

        //       if( newIScore > iScore ) {
        //         setIScore( newIScore )
        //         children = Some( SpannedChildren( headEntry, Some( argEntry ) ) )
        //       }

        //     }
        //   }
        //   class RightwardLexEntry( h:SingleLabel, span:Span ) extends RightwardEntry( h, span ) {
        //     iScore = 0D
        //     children = None
        //     override def dependencyParse = Set[DirectedArc]()
        //     override def constituencyParse = "(" + h + " " + h.obs + " )"
        //   }


        //   // def rootEntry = matrix( 0 )( s.length )( rootLabel )
        //   def treeScore = {
        //     matrix( 0 )( 2*s.length )( StartLabel ).iScore
        //   }


        //   // Re-wrote lexFill so we don't explicitly touch iScore at all, only add entries.
        //   def lexFill( index:Int ) {

        //       // these are lexical entries so we are guaranteed to have no deps yet, hence v = true
        //     if( index%2 == 0 ) {
        //       val w = s(index/2)
        //       val leftward = LeftwardLabel( w, true )
        //       addEntry( new LeftwardLexEntry( leftward, Span( index, index+1 ) ) )
        //     } else {
        //       val w = s((index-1)/2)
        //       val rightward = RightwardLabel( w, true )
        //       addEntry( new RightwardLexEntry( rightward, Span( index, index+1 ) ) )
        //     }
        //   }

        //   def synFill( i:Int, j:Int ) {
        //     //println( "synFill for " + (i, j) )


        //     ( (i+1) to (j-1) ).foreach{ k =>

        //       // println( (i,k,j) + " " + (i%2==0, k%2==0, j%2==0) )

        //       (i%2==0, k%2==0, j%2==0) match {
        //         case (true, false, true) => {
        //           // matrix(i)(k): leftward
        //           // matrix(k)(j): rightward
        //           // action: Add root if i == 0 and j == 2*s.length
        //           if( i == 0 && j == (2*s.length) ) {
        //             val t = (k-1)/2
        //             val left = LeftwardLabel( s(t), t == 0 )
        //             val right = RightwardLabel( s(t), (t+1) == s.length )
        //             val rootLabel = RootLabel( left, right )

        //             addRootEntry( new RootEntry( rootLabel, Span( 0, 2*s.length ) ) )
        //           }

        //         }
        //         case (false, true, false) => {
        //           // matrix(i)(k): rightward
        //           // matrix(k)(j): leftward
        //           // action: add M node

        //           val rightwardHeads = matrix(i)(k).keySet
        //           val leftwardHeads = matrix(k)(j).keySet
        //           assert(
        //             rightwardHeads.forall{ _ match { case RightwardLabel(_,_) => true ; case _ => false } }
        //           )
        //           assert(
        //             leftwardHeads.forall{ _ match { case LeftwardLabel(_,_) => true ; case _ => false } }
        //           )

        //           (
        //             for {left <- rightwardHeads ; right <- leftwardHeads } yield (left, right )
        //           ).foreach{ case( left:RightwardLabel, right:LeftwardLabel ) =>
        //             val split = SplitLabel( left, right )

        //             if( ! matrix( i )( j ).keySet.contains( split ) )
        //               addEntry( new SplitEntry( split, Span( i, j ) ) )

        //             matrix( i )( j )( split ).addDependency(
        //                 matrix( i )( k )( left ), matrix( k )( j )( right )
        //                 // matrix( i )( k )( left ).iScore + matrix( k )( j )( right ).iScore
        //             )
        //           }
        //         }
        //         case (false, false, true) => {
        //           // matrix(i)(k): M node
        //           // matrix(k)(j): rightward
        //           // action: rightward attachment
        //           val splits = matrix(i)(k).keySet
        //             /// println( splits.mkString( "< ", ", ", " >" ) )
        //           assert(
        //             splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
        //           )
        //           splits.foreach{ split =>
        //             val SplitLabel( leftHead, rightHead ) = split

        //             val leftParent = RightwardLabel( leftHead.obs, false )
        //             val rightArg = RightwardLabel( rightHead.obs, ( 2*(rightHead.obs.t+1) ) == j )

        //             if( ! matrix( i )( j ).keySet.contains( leftParent ) )
        //               addEntry( new RightwardEntry( leftParent, Span( i, j ) ) )
        //             matrix( i )( j )( leftParent ).addDependency(
        //               matrix( i )( k )( split ),
        //               matrix( k )( j )( rightArg )
        //             )
        //           }
        //         }
        //         case (true, false, false) => {
        //           // matrix(i)(k): leftward
        //           // matrix(k)(j): M node
        //           // action: leftward attachment
        //           val splits = matrix(k)(j).keySet
        //           assert(
        //             splits.forall{ _ match { case SplitLabel(_,_) => true ; case _ => false } }
        //           )
        //           splits.foreach{ split =>
        //             val SplitLabel( leftHead, rightHead ) = split

        //             val rightParent = LeftwardLabel( rightHead.obs, false )
        //             val leftArg = LeftwardLabel( leftHead.obs, (2*leftHead.obs.t) == i )

        //             if( ! matrix( i )( j ).keySet.contains( rightParent ) )
        //               addEntry( new LeftwardEntry( rightParent, Span( i, j ) ) )
        //             matrix( i )( j )( rightParent ).addDependency(
        //               matrix( k )( j )( split ),
        //               matrix( i )( k )( leftArg )
        //             )
        //           }
        //         }
        //         case (true, true, true) => {
        //           // matrix(i)(k): nothing
        //           // matrix(k)(j): nothing
        //           // action: nothing
        //         }
        //         case (true, true, false) => {
        //           // matrix(i)(k): nothing
        //           // matrix(k)(j): leftward
        //           // action: nothing
        //         }
        //         case (false, true, true) => {
        //           // matrix(i)(k): rightward
        //           // matrix(k)(j): nothing
        //           // action: nothing
        //         }
        //         case (false, false, false) => {
        //           // matrix(i)(k): M node
        //           // matrix(k)(j): M node
        //           // action: nothing
        //         }
        //       }


        //     }

        //   }

        //   def size = 2*s.size

        //   def toDependencyParse = matrix(0)(size)(StartLabel).dependencyParse.toList.sortWith{
        //     _.arg.t < _.arg.t }.map{_.head.t}.mkString( "[ ", ", ", " ] " )
        //   def toConstituencyParse = matrix(0)(size)(StartLabel).constituencyParse

        //   override def toString = {
        //     matrix.map{ row =>
        //       row.map{ x =>
        //         if( x != null )
        //           x.values.mkString( "\n", "\n", "\n" )
        //       }.mkString("\n","\n","\n")
        //     }.mkString("\n","\n","\n\n")
        //   }


        // }

  /*
  * This is the CYK parsing algorithm.
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with inside and outside probabilities.
  */
  def populateChart( s:List[TimedObservedLabel] ) = {
    // val chart = new ViterbiChart( s )
    val chart =
      if( s.last != FinalRoot( s.length ) )
        new ViterbiChart( s :+ FinalRoot( s.length ) )
      else
        new ViterbiChart( s )

    (1 to ( chart.size )) foreach{ j =>
      chart.lexFill( j-1 )
      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          chart.synFill( i , j )
        }
    }

    chart
  }

        // /*
        // * This is the CYK parsing algorithm with the Fold-Unfold transform (Johnson, 2007)
        // * @param s The input sentence (an array of terminals)
        // * @return A parse chart with inside and outside probabilities.
        // */
        // def efficientPopulateChart( s:List[TimedObservedLabel] ) = {
        //   println( s.mkString( "[ ", ", ", " ]" ) )
        //   val chart = new FoldUnfoldVitChart( s )

        //   (1 to ( chart.size )) foreach{ j =>
        //     chart.lexFill( j-1 )
        //     if( j > 1 )
        //       (0 to (j-2)).reverse.foreach{ i =>
        //         println( (i, j ) )
        //         chart.synFill( i , j )
        //       }
        //   }

        //   chart
        // }


  def constituencyParse( toParse: List[TimedSentence] ) =
    toParse.map{ case TimedSentence( id, s ) =>
      val chart = populateChart( s )
      id + " " + chart.toConstituencyParse
    }

  def dependencyParse( toParse: List[TimedSentence] ) =
    toParse.map{ case TimedSentence( id, s ) =>
      val chart = populateChart( s )
      id + " " + chart.toDependencyParse
    }

  def bothParses( toParse: List[AbstractTimedSentence], prefix:String ) =
    toParse.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
          prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
          prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        }
        case TimedThreeStreamSentence( id, s ) => {
          val chart = populateChart( s )
          prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
          prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        }
      }
    }

  def bothParsesWithLogProb( toParse: List[AbstractTimedSentence], prefix:String ) =
    toParse.map{ _ match {
        case TimedSentence( id, s ) => {
          val chart = populateChart( s )
          val ScoredParse( dep, viterbiParseLogProb ) = chart.toDependencyParseWithLogProb
          val ScoredParse( con, _ ) = chart.toConstituencyParseWithLogProb
          prefix + ":score:" + id + " " + viterbiParseLogProb + "\n" +
          prefix + ":dependency:" + id + " " + dep + "\n" +
          prefix + ":constituency:" + id + " " + con
        }
        case TimedTwoStreamSentence( id, s ) => {
          val chart = populateChart( s )
          val ScoredParse( dep, viterbiParseLogProb ) = chart.toDependencyParseWithLogProb
          val ScoredParse( con, _ ) = chart.toConstituencyParseWithLogProb
          prefix + ":score:" + id + " " + viterbiParseLogProb + "\n" +
          prefix + ":dependency:" + id + " " + dep + "\n" +
          prefix + ":constituency:" + id + " " + con
        }
        case TimedThreeStreamSentence( id, s ) => {
          val chart = populateChart( s )
          val ScoredParse( dep, viterbiParseLogProb ) = chart.toDependencyParseWithLogProb
          val ScoredParse( con, _ ) = chart.toConstituencyParseWithLogProb
          prefix + ":score:" + id + " " + viterbiParseLogProb + "\n" +
          prefix + ":dependency:" + id + " " + dep + "\n" +
          prefix + ":constituency:" + id + " " + con
        }
      }
    }


        // def efficientBothParses( toParse: List[AbstractTimedSentence], prefix:String ) =
        //   toParse.map{ _ match {
        //       case TimedSentence( id, s ) => {
        //         val chart = efficientPopulateChart( s )
        //         prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
        //         prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        //       }
        //       case TimedTwoStreamSentence( id, s ) => {
        //         val chart = efficientPopulateChart( s )
        //         prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
        //         prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        //       }
        //       case TimedThreeStreamSentence( id, s ) => {
        //         val chart = efficientPopulateChart( s )
        //         prefix + ":dependency:" + id + " " + chart.toDependencyParse + "\n" +
        //         prefix + ":constituency:" + id + " " + chart.toConstituencyParse
        //       }
        //     }
        //   }
}

