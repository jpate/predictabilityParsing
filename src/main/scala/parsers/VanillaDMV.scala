package predictabilityParsing.parsers

import predictabilityParsing.grammars.AbstractDMVGrammar
import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log


class VanillaDMVEstimator /*( vocab:Set[ObservedLabel] )*/ extends AbstractDMVParser{
  val g:AbstractDMVGrammar = new DMVGrammar//( vocabulary = vocab )

  //def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

  def setHarmonicGrammar[O<:TimedObservedLabel](
    corpus:List[List[O]],
    rightFirst:Double = 0.75,
    cAttach:Double = 15.0,
    cStop:Double = 3.0,
    cNotStop:Double = 1.0,
    stopUniformity:Double = 20.0
  ) {
    val pc = g.emptyPartialCounts //new DMVPartialCounts

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

    pc.setStopCounts(
      StopOrNot( Root, LeftAttachment, true ),
      NotStop,
      0D
    )
    pc.setStopCounts(
      StopOrNot( Root, LeftAttachment, true ),
      Stop,
      Double.NegativeInfinity
    )

    pc.setStopCounts(
      StopOrNot( Root, LeftAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )
    pc.setStopCounts(
      StopOrNot( Root, LeftAttachment, false ),
      Stop,
      0D
    )


    println( "setting harmonic initialization:" )
    setGrammar( pc.toDMVGrammar )
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

      var iScore:Double = label.mark match {
        case UnsealedLeftFirst =>
          if( span.end - span.start == 1 )
            g.orderScore( label.obs.w, LeftFirst )
          else
            Double.NegativeInfinity
        case UnsealedRightFirst =>
          if( span.end - span.start == 1 )
            g.orderScore( label.obs.w, RightFirst )
          else
            Double.NegativeInfinity
        case SealedRight | SealedLeft | Sealed =>
          (label.peel.toSet & matrix(span.start)(span.end).keySet).foldLeft(
          Double.NegativeInfinity) { (x, peeledH ) =>
            Math.sumLogProb(
              x,
              g.stopScore( StopOrNot( label.obs.w, peeledH.attachmentDirection, adj( peeledH, span ) ), Stop ) +
                matrix(span.start)(span.end)( peeledH ).iScore
            )
          }
      }

      var oScore = Double.NegativeInfinity
      var score = Double.NegativeInfinity
      def computeMarginal{ score = oScore + iScore }

      def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
      def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

      def setOScore( x:Double ) { oScore = x }

      var children = Set[SpannedChildren]()

      def addDependency( headEntry:Entry, argEntry:Entry ) {
        val h = headEntry.label
        assert( label == h )
        val a = argEntry.label
        incrementIScore(
          g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) +
          g.chooseScore( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) +
          argEntry.iScore +
          headEntry.iScore
        )
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
                  Math.sumLogProb( bestHead.score, bestArg.get.score )
              } > {
                if( newArg.isEmpty )
                  newHead.score
                else
                  Math.sumLogProb( newHead.score, newArg.get.score )
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
                  Math.sumLogProb( bestHead.score, bestArg.get.score )
              } > {
                if( newArg.isEmpty )
                  newHead.score
                else
                  Math.sumLogProb( newHead.score, newArg.get.score )
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
    }


    def rootEntry = matrix( 0 )( s.length )( MarkedObservation( FinalRoot(s.length-1), Sealed ) )
    def treeScore =
      rootEntry.iScore
      //matrix( 0 )( s.length )( MarkedObservation( FinalRoot( s.length-1 ), Sealed ) ).iScore

    def toMaxMarginalDependencyParse =
      rootEntry.maxMarginalDependencyParse.toList.sortWith{ _.arg.t < _.arg.t }.map{_.head.t}.mkString( "[ ", ", ", " ] " )
    def toMaxMarginalConstituencyParse = rootEntry.maxMarginalConstituencyParse


    // Re-wrote lexFill so we don't explicitly touch iScore at all, only add entries.
    def lexFill( index:Int ) {
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
      // First, let's just add all the possible heads with possible dependencies. Since we are
      // guaranteed that end-start>1, we cannot have any seals until we have added heads to the
      // current span.

      ( (start+1) to (end-1) ).foreach{ k =>
        val rightArguments = matrix(k)(end).keySet.filter{ _.mark == Sealed }
        val leftArguments = matrix(start)(k).keySet.filter{ _.mark == Sealed }

        val unsealedLeftHeads = matrix(start)(k).keySet.filter{ _.mark == UnsealedRightFirst }
        unsealedLeftHeads.foreach{ h =>
          if( !( matrix(start)(end).contains( h ) ) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
          rightArguments.foreach( a =>
            matrix(start)(end)(h).addDependency(
              matrix(start)(k)(h),
              matrix(k)(end)(a)
            )
          )
        }

        val unsealedRightHeads = matrix(k)(end).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>
          if( !( matrix(start)(end).contains( h ) ) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )

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
          if( !( matrix(start)(end).contains( h ) ) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
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
          if( !( matrix(start)(end).contains( h ) ) )
            matrix(start)(end) += h -> new Entry( h, Span(start,end) )
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
        if( !( matrix(start)(end).contains( h.seal.get ) ) )
          matrix(start)(end) += h.seal.get -> new Entry( h.seal.get, Span(start,end) )
      )
      matrix(start)(end).keySet.filter{ h =>
        (h.mark == SealedLeft ) | (h.mark == SealedRight)
      }.foreach( h =>
        if( !( matrix(start)(end).contains( h.seal.get ) ) )
          matrix(start)(end) += h.seal.get -> new Entry( h.seal.get, Span(start,end) )
      )
    }

    def outsidePass {
      import math.exp
      val n = s.length

      //matrix( 0 )( n )( MarkedObservation( FinalRoot(n-1), Sealed ) ).setOScore( 0D )
      rootEntry.setOScore( 0D )
      // 1 to (n) rather than 1 to (n-1) because we do have unary branches over the whole sentence
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
      //println( "STARTING toPartialCounts for " + s.mkString("[ ", ", ", " ]" ) )
      //val pc = new DMVPartialCounts
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

      // pc.divideChooseCounts( treeScore )
      // pc.divideStopCounts( treeScore )
      // pc.divideOrderCounts( treeScore )

      pc.setTotalScore( treeScore )
      //println( "\n\n ---  DONE WITH toPartialCounts  ---\n\n" )
      pc
    }

    def computeMarginals {
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          matrix(i)(j).values.foreach( _.computeMarginal )
        }
      }
    }

    def size = s.size

    override def toString =
      matrix.map{ row =>
        row.map{ x =>
          if( x != null )
            x.values.mkString( "\n", "\n", "\n" )
        }.mkString("\n","\n","\n")
      }.mkString("\n","\n","\n\n")
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

    // println( "Chart for " + s.mkString( "[ ", ", ", " ]" ) )
    // println( chart + "\n\n\n\n\n" )

    chart
  }

  def maxMarginalParse( corpus:List[AbstractTimedSentence], prefix:String ) =
    corpus.par.map{ _ match {
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
    // corpus.par.map{ s =>
    //   val pc = populateChart(s).toPartialCounts
    //   pc
    // }.reduceLeft{(a,b) =>
    //   a.destructivePlus(b);
    //   a
    // }

  def computePartialCounts( corpus:Iterable[List[TimedObservedLabel]] ) =
    corpus.par.map{ s =>
      val pc = populateChart(s).toPartialCounts
      pc
    //}.toList.reduceLeft{(a,b) =>
    }.toList.foldLeft( g.emptyPartialCounts ){(a,b) =>
      a.destructivePlus(b);
      a
    }

}

class VanillaDMVParser extends AbstractDMVParser{
  import scala.util.Random
  private val r = new Random(10) // 10 for now

  val g:AbstractDMVGrammar = new DMVGrammar//( vocabulary = Set[ObservedLabel]() )

  def setGrammar( givenGrammar:DMVGrammar ) {
    g.setParams( givenGrammar.getVanillaParams )
    //println( "viterbi grammar is:\n" + g )
  }
  //def setGrammar( givenGrammar:DMVGrammar ) { g = givenGrammar }

  //def setGrammar( grammarParams:DMVParameters ) { g.setParams( grammarParams ) }


  class ViterbiChart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,Entry]()
    )

    def apply( start:Int, end:Int ) = matrix(start)(end)


    // Ok, stupidly simple entry class
    abstract class Entry(
      val label:MarkedObservation,
      val score:Double,
      val headChild:Option[Entry],
      val argChild:Option[Entry]
    ) {
      def constituencyParse:String
      def dependencyParse:Set[DirectedArc]
    }

    object NullEntry extends Entry(
      //MarkedObservation( FinalRoot(0), Sealed ),
      MarkedObservation( InitialRoot, Sealed ),
      Double.NegativeInfinity,
      None,
      None
    ) {
      def constituencyParse:String = ""
      def dependencyParse:Set[DirectedArc] = Set()
    }

    class UnaryEntry(
      label:MarkedObservation,
      score:Double,
      headChild:Entry
    ) extends Entry( label, score, Some(headChild), None ) {
      def constituencyParse = "(" + label + " " + headChild.constituencyParse + " )"
      def dependencyParse = headChild.dependencyParse
    }

    class TerminalEntry( label:MarkedObservation, score:Double )
      extends Entry( label, score, None, None ) {
      def constituencyParse = "(" + label + " " + label.obs + ")"
      def dependencyParse = Set[DirectedArc]()
    }

    abstract class BinaryEntry(
      headChild:Entry,
      argChild:Entry,
      score:Double
    ) extends Entry( headChild.label, score, Some(headChild), Some(argChild) ) {
      def dependencyParse =
        headChild.dependencyParse ++
          argChild.dependencyParse +
            DirectedArc(headChild.label.obs, argChild.label.obs )
    }

    class LeftHeadedEntry(
      headChild:Entry,
      argChild:Entry,
      score:Double
    ) extends BinaryEntry( headChild, argChild, score ) {
      def constituencyParse =
        "(" + headChild.label + " " +
          headChild.constituencyParse + " " + argChild.constituencyParse +
        " ) "
    }

    class RightHeadedEntry(
      headChild:Entry,
      argChild:Entry,
      score:Double
    ) extends BinaryEntry( headChild, argChild, score ) {
      def constituencyParse =
        "(" + headChild.label + " " +
          argChild.constituencyParse + " " + headChild.constituencyParse +
        " ) "
    }


    def lexFill( index:Int ) {
      val w = s(index)


      if( g.orderScore( w.w , LeftFirst ) > g.orderScore( w.w , RightFirst ) ) {
        val unsealedLeftFirst = MarkedObservation( w, UnsealedLeftFirst )
        matrix(index)(index+1) +=
          unsealedLeftFirst -> new TerminalEntry( unsealedLeftFirst, g.orderScore( w.w, LeftFirst ) )

        val sealedLeft = unsealedLeftFirst.seal.get
        matrix(index)(index+1) +=
          sealedLeft -> new TerminalEntry(
            sealedLeft,
            matrix(index)(index+1)(unsealedLeftFirst).score +
              g.stopScore( StopOrNot( w.w, LeftAttachment, true ), Stop )
          )

        val sealedBoth = sealedLeft.seal.get
        matrix(index)(index+1) +=
          sealedBoth -> new TerminalEntry(
            sealedBoth,
            matrix(index)(index+1)(sealedLeft).score +
              g.stopScore( StopOrNot( w.w, RightAttachment, true ), Stop )
          )

      } else {
        val unsealedRightFirst = MarkedObservation( w, UnsealedRightFirst )
        matrix(index)(index+1) +=
          unsealedRightFirst -> new TerminalEntry( unsealedRightFirst, g.orderScore( w.w, RightFirst ) )

        val sealedRight = unsealedRightFirst.seal.get
        matrix(index)(index+1) +=
          sealedRight -> new TerminalEntry(
            sealedRight,
            matrix(index)(index+1)(unsealedRightFirst).score +
              g.stopScore( StopOrNot( w.w, RightAttachment, true ), Stop )
          )

        val sealedBoth = sealedRight.seal.get
        matrix(index)(index+1) +=
          sealedBoth -> new TerminalEntry(
            sealedBoth,
            matrix(index)(index+1)(sealedRight).score +
              g.stopScore( StopOrNot( w.w, LeftAttachment, true ), Stop )
          )
      }
      //println( (index,index+1) + ": " + matrix(index)(index+1).keySet )
    }

    def synFill( start:Int, end:Int ) {
      // First, let's just add all the possible heads with possible dependencies. Since we are
      // guaranteed that end-start>1, we cannot have any seals until we have added heads to the
      // current span.

      ( (start+1) to (end-1) ).foreach{ k =>
        val rightArgs = matrix(k)(end).keySet.filter{ _.mark == Sealed }
        val leftArgs = matrix(start)(k).keySet.filter{ _.mark == Sealed }

        val unsealedLeftHeads = matrix(start)(k).keySet.filter{ _.mark == UnsealedRightFirst }
        //println( unsealedLeftHeads.mkString( "{ ", ", ", " }" ) )
        unsealedLeftHeads.foreach{ h =>

          val firstRightArg = rightArgs.head
          val firstRightArgScore = 
            g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ), NotStop ) +
            g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , firstRightArg.obs.w ) +
            matrix(start)(k)(h).score +
            matrix(k)(end)(firstRightArg).score
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.tail.foldLeft(
              Tuple2( firstRightArg, firstRightArgScore )
              //Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ), NotStop ) +
                g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              if( newScore >= bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          if( bestArgScore >= matrix(start)(end).getOrElse( h , NullEntry ).score ) {
            matrix(start)(end) +=
              h -> new LeftHeadedEntry(
                matrix(start)(k)(h),
                matrix(k)(end)(bestArg),
                bestArgScore
              )
          }
        }

        val unsealedRightHeads = matrix(k)(end).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>

          val firstLeftArg = leftArgs.head
          val firstLeftArgScore = 
            g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ), NotStop ) +
            g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , firstLeftArg.obs.w ) +
            matrix(start)(k)(firstLeftArg).score +
            matrix(k)(end)(h).score
          val Tuple2( bestArg, bestArgScore ) =
            leftArgs.tail.foldLeft(
              Tuple2( firstLeftArg, firstLeftArgScore )
              //Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ), NotStop ) +
                g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , arg.obs.w ) +
                matrix(start)(k)(arg).score +
                matrix(k)(end)(h).score

              if( newScore >= bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          if( bestArgScore >= matrix(start)(end).getOrElse( h , NullEntry ).score ) {
            matrix(start)(end) +=
              h -> new RightHeadedEntry(
                matrix(k)(end)(h),
                matrix(start)(k)(bestArg),
                bestArgScore
              )
          }
        }



        val halfSealedLeftHeads = matrix(start)(k).keySet.filter{ _.mark == SealedLeft }
        halfSealedLeftHeads.foreach{ h =>
          val firstRightArg = rightArgs.head
          val firstRightArgScore = 
            g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ), NotStop ) +
            g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , firstRightArg.obs.w ) +
            matrix(start)(k)(h).score +
            matrix(k)(end)(firstRightArg).score
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.tail.foldLeft(
              Tuple2( firstRightArg, firstRightArgScore )
              //Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.stopScore( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ), NotStop ) +
                g.chooseScore( ChooseArgument( h.obs.w, RightAttachment ) , arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              if( newScore >= bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          if( bestArgScore >= matrix(start)(end).getOrElse( h , NullEntry ).score ) {
            matrix(start)(end) +=
              h -> new LeftHeadedEntry(
                matrix(start)(k)(h),
                matrix(k)(end)(bestArg),
                bestArgScore
              )
          }
        }

        val halfSealedRightHeads = matrix(k)(end).keySet.filter{ _.mark == SealedRight }
        halfSealedRightHeads.foreach{ h =>
          val firstLeftArg = leftArgs.head
          val firstLeftArgScore =
            g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ), NotStop ) +
            g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , firstLeftArg.obs.w ) +
            matrix(start)(k)(firstLeftArg).score +
            matrix(k)(end)(h).score
          val Tuple2( bestArg, bestArgScore ) =
            leftArgs.tail.foldLeft(
              Tuple2( firstLeftArg, firstLeftArgScore )
              //Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.stopScore( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ), NotStop ) +
                g.chooseScore( ChooseArgument( h.obs.w, LeftAttachment ) , arg.obs.w ) +
                matrix(start)(k)(arg).score +
                matrix(k)(end)(h).score

              if( newScore >= bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          if( bestArgScore >= matrix(start)(end).getOrElse( h , NullEntry ).score ) {
            matrix(start)(end) +=
              h -> new RightHeadedEntry(
                matrix(k)(end)(h),
                matrix(start)(k)(bestArg),
                bestArgScore
              )
          }
        }

      }

      matrix(start)(end).keySet.filter{ _.mark.sealCount == 0 }.foreach{ h =>
        assert( !( matrix(start)(end).contains( h.seal.get ) ) )
        matrix(start)(end) +=
          h.seal.get -> new UnaryEntry(
            h.seal.get,
            matrix(start)(end)(h).score +
              g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span( start,end) ) ), Stop ),
            matrix(start)(end)(h)
          )
      }

      matrix(start)(end).keySet.filter{ _.mark.sealCount == 1 }.foreach{ h =>
        assert( !( matrix(start)(end).contains( h.seal.get ) ) )
        matrix(start)(end) +=
          h.seal.get -> new UnaryEntry(
            h.seal.get,
            matrix(start)(end)(h).score +
              g.stopScore( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span( start,end) ) ), Stop ),
            matrix(start)(end)(h)
          )
      }

      //println( (start,end) + ": " + matrix(start)(end).keySet )
    }


    def size = s.size

    def toConstituencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length-1 ) , Sealed ) ).constituencyParse

    def toDependencyParse =
      matrix(0)(s.length)(
        MarkedObservation( FinalRoot( s.length-1 ) , Sealed )
      ).dependencyParse.toList.sortWith{ _.arg.t < _.arg.t }.map{_.head.t}.mkString( "[ ", ", ", " ] " )

  }

  /*
  * This is the CYK parsing algorithm.
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with inside and outside probabilities.
  */
  def populateChart( s:List[TimedObservedLabel] ) = {
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
      }
    }
}
