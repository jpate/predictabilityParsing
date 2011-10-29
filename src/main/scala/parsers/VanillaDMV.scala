package predictabilityParsing.parsers

import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log


class VanillaDMVEstimator( vocab:Set[ObservedLabel] ) extends AbstractDMVParser{
  val g = new DMVGrammar( vocabulary = vocab )

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }


  class Chart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,Entry]()
    )

    def apply( start:Int, end:Int ) = matrix(start)(end)


    // Ok, stupidly simple entry class
    class Entry( val label:MarkedObservation, val span:Span ) {

      var iScore:Double = label.mark match {
        case UnsealedLeftFirst =>
          if( span.end - span.start == 1 )
            g.p_order( label.obs.w, LeftFirst )
          else
            Double.NegativeInfinity
        case UnsealedRightFirst =>
          if( span.end - span.start == 1 )
            g.p_order( label.obs.w, RightFirst )
          else
            Double.NegativeInfinity
        case SealedRight | SealedLeft | Sealed =>
          (label.peel.toSet & matrix(span.start)(span.end).keySet).map( peeledH =>
            g.p_stop( StopOrNot( label.obs.w, peeledH.attachmentDirection, adj( peeledH, span ) ), Stop ) +
              matrix(span.start)(span.end)( peeledH ).iScore
          ).foldLeft(Double.NegativeInfinity)( Math.sumLogProb(_,_) )
      }
      var oScore = Double.NegativeInfinity
      var score = Double.NegativeInfinity
      def computeMarginal( treeScore:Double ) { score = oScore + iScore }//- treeScore }

      def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
      def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

      def setOScore( x:Double ) { oScore = x }


      def addDependency( headEntry:Entry, argEntry:Entry ) {
        //dependents += argEntry
        val h = headEntry.label
        assert( label == h )
        val a = argEntry.label
        incrementIScore(
          g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, headEntry.span ) ) , NotStop ) +
          g.p_choose( ChooseArgument( h.obs.w, h.attachmentDirection ) , a.obs.w ) +
          argEntry.iScore +
          headEntry.iScore
        )
      }
    }

    class TerminalEntry( h:MarkedObservation, index:Int ) extends Entry( h, Span( index, index +1 ) )


    def treeScore =
      matrix( 0 )( s.length )( MarkedObservation( FinalRoot( s.length-1 ), Sealed ) ).iScore


    // Re-write lexFill so we don't explicitly touch iScore at all, only add entries.
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

    // Re-write synFill so we don't explicitly touch iScore at all, only add entries and dependents.
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

      matrix(start)(end).keySet.filterNot{ _.mark == Sealed }.foreach( h =>
        if( !( matrix(start)(end).contains( h.seal.get ) ) )
          matrix(start)(end) += h.seal.get -> new Entry( h.seal.get, Span(start,end) )
      )
    }

    def outsidePass {
      import math.exp
      val n = s.length

      matrix( 0 )( n )( MarkedObservation( FinalRoot(n-1), Sealed ) ).setOScore( 0D )
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
                  g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj(h, Span(k,i) ) ) , NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
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
                  g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj(h,Span(j,k)) ) , NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                  matrix( j )( k )( h ).iScore +
                  matrix( i )( k )( h ).oScore
                )
              }

            }
          }

          // Now, half-sealed to the left
          val halfSealedLeft = matrix(i)(j).keySet.filter{ _.mark == SealedLeft }
          halfSealedLeft.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, RightAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, Span(i,k) ) ), NotStop ) +
                  //g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.p_choose( ChooseArgument( w.obs.w, RightAttachment ) , a.obs.w ) + 
                      matrix(i)(k)(w).oScore + matrix(j)(k)(a).iScore
                )
              }
            }
          }

          // Now half-sealed to the right
          val halfSealedRight = matrix(i)(j).keySet.filter{ _.mark == SealedRight }
          halfSealedRight.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.p_stop(StopOrNot( w.obs.w, LeftAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, Span(k,j) ) ), NotStop ) +
                  //g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.p_choose( ChooseArgument( w.obs.w, LeftAttachment ) , a.obs.w ) + 
                      matrix(k)(j)(w).oScore + matrix(k)(i)(a).iScore
                )
              }
            }
          }

          // On to unsealed. First, unsealed, left-first
          val unsealedLeftFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedLeftFirst }
          unsealedLeftFirst.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, LeftAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, Span(k,j) ) ), NotStop ) +
                  //g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.p_choose( ChooseArgument( w.obs.w, LeftAttachment ) , a.obs.w ) + 
                      matrix(k)(j)(w).oScore + matrix(k)(i)(a).iScore
                )
              }
            }
          }

          // Now, unsealed, right-first
          val unsealedRightFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedRightFirst }
          unsealedRightFirst.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, RightAttachment , adj( w, curSpan ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, Span(i,k) ) ), NotStop ) +
                  //g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, curSpan ) ), NotStop ) +
                    g.p_choose( ChooseArgument( w.obs.w, RightAttachment ) , a.obs.w ) + 
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
      val pc = new DMVPartialCounts


      ( 0 to (s.length-1 ) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          val curSpan = Span(i,j)
          matrix(i)(j).values.foreach( _.computeMarginal( treeScore ) )

          // increment numerators and denominators for every opportunity to seal
          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            pc.incrementStopCounts(
              StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ),
              Stop,
              matrix(i)(j)(h.seal.get).score /*+
                g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ), Stop ) */
            )
            // pc.incrementStopDenomCounts(
            //   StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ),
            //   matrix(i)(j)(h).score
            // )
          }
        }
      }


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
          val curSpan = Span( i,j)
          // count everything up. We'll do the division through one normalization step at the end.

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
                  g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(i,k) ) ) , NotStop ) +
                  //g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, curSpan ) ) , NotStop ) +
                    g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                      //matrix(i)(k)(h).score
                      matrix(i)(k)(h).iScore + matrix(k)(j)(a).iScore + matrix(i)(j)(h).oScore
                )
                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span(i,k)) ),
                  Stop,
                  matrix(i)(j)(h.seal.get).score /*+
                    g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ), Stop ) */
                )
                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span(i,k)) ),
                  NotStop,
                  matrix(i)(j)(h).score /*+
                    g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ), Stop ) */
                )
              }
            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                pc.incrementChooseCounts(
                  ChooseArgument( h.obs.w, LeftAttachment ),
                  a.obs.w,
                  g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,j) ) ) , NotStop ) +
                    g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      //matrix(k)(j)(h).score
                      matrix(i)(k)(a).iScore + matrix(k)(j)(h).iScore + matrix(i)(j)(h).oScore
                )
                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span(k,j)) ),
                  Stop,
                  matrix(i)(j)(h.seal.get).score /*+
                    g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ), Stop ) */
                )
                pc.incrementStopCounts(
                  StopOrNot( h.obs.w, h.attachmentDirection, adj( h, Span(k,j) ) ),
                  NotStop,
                  matrix(i)(j)(h).score /*+
                    g.p_stop( StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ), Stop ) */
                )
              }
            }
          }
        }
      }

      pc.setTotalScore( treeScore )
      pc
    }

    def size = s.size

    // override def toString =
    //   matrix.map{ row =>
    //     row.map{ x => if( x == null ) "    " else x }.mkString("<",">\t\t<",">\n") +
    //     row.map{ x => if( x == null ) "    " else x.iScore }.mkString("<",">\t\t<",">\n") +
    //     row.map{ x => if( x == null ) "    " else x.oScore }.mkString("<",">\t\t<",">\n\n")
    //   }.mkString("\n","\n","\n\n")
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
    chart
  }

  def computePartialCounts( corpus:Iterable[List[TimedObservedLabel]] ) =
    corpus/*.par*/.map{ s =>
      val pc = populateChart(s).toPartialCounts
      pc
    }.reduce{(a,b) => a.destructivePlus(b); a}

}


class VanillaDMVParser extends AbstractDMVParser {
  import scala.util.Random
  private val r = new Random(10) // 10 for now

  val g = new DMVGrammar( vocabulary = Set[ObservedLabel]() )

  class ViterbiChart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,AbstractVitEntry]()
    )

    def apply( start:Int, end:Int ) = matrix(start)(end)


    abstract class AbstractVitEntry(
      val headChild:Option[AbstractVitEntry],
      val headLabel:MarkedObservation,
      val argChild:Option[AbstractVitEntry],
      val score:Double ) {
      def constituencyParse:String
      def dependencyParse:Set[DirectedArc]
    }

    class ViterbiLex( lexChild:Option[ViterbiLex], lexLabel:MarkedObservation, score:Double )
      extends AbstractVitEntry( lexChild, lexLabel, None, score ) {
      def constituencyParse =
        "(" + lexLabel + " " +
          { if( lexChild.isEmpty ) lexLabel.obs else lexChild.get.constituencyParse } + ")"
      def dependencyParse = Set[DirectedArc]()
    }

    class ViterbiSealed( h:Option[AbstractVitEntry], score:Double )
      extends AbstractVitEntry( h, h.get.headLabel, None, score ) {
      def constituencyParse =
        "(" + MarkedObservation( h.get.headLabel.obs, Sealed ) + " " + h.get.constituencyParse + " )"
      def dependencyParse = h.get.dependencyParse
    }

    abstract class AbstractVitSynEntry(
      h:Option[AbstractVitEntry],
      val attachDir:AttachmentDirection,
      a:Option[AbstractVitEntry],
      score:Double
    ) extends AbstractVitEntry( h, h.get.headLabel, a, score ) {
      def dependencyParse =
        h.get.dependencyParse ++ {
          if( a.isEmpty )
            Set[DirectedArc]()
          else
            Set(
              DirectedArc( h.get.headLabel.obs, a.get.headLabel.obs )
            ) ++ a.get.dependencyParse
        }
    }

    class LeftHeadedVitSynEntry(
      h:Option[AbstractVitEntry],
      a:Option[AbstractVitEntry],
      score:Double
    ) extends AbstractVitSynEntry( h, RightAttachment, a, score ) {
      def constituencyParse =
        "(" + h.get.headLabel +
              " " + h.get.constituencyParse +
            { if( a.isEmpty ) "" else  " " + a.get.constituencyParse } +
        " )"
    }
    class RightHeadedVitSynEntry(
      h:Option[AbstractVitEntry],
      a:Option[AbstractVitEntry],
      score:Double
    ) extends AbstractVitSynEntry( h, LeftAttachment, a, score ) {
      def constituencyParse =
        "(" + h.get.headLabel +
          { if( a.isEmpty ) "" else  " " + a.get.constituencyParse } +
            " " + h.get.constituencyParse +
        " )"
    }

    def lexFill( index:Int ) {
      val w = s(index)

      // First, pick best order.
      val Tuple2( bestOrder, bestOrderScore ) =
        if( g.p_order( w.w )( LeftFirst ) > g.p_order( w.w , RightFirst ) )
          Tuple2( UnsealedLeftFirst, g.p_order( w.w , LeftFirst ) )
        else if( g.p_order( w.w )( LeftFirst ) < g.p_order( w.w , RightFirst ) )
          Tuple2( UnsealedRightFirst, g.p_order( w.w , RightFirst ) )
        else
          if( r.nextDouble >= 0.5 )
            Tuple2( UnsealedLeftFirst, g.p_order( w.w , LeftFirst ) )
          else
            Tuple2( UnsealedRightFirst, g.p_order( w.w , RightFirst ) )

      val selectedOrder = MarkedObservation( w, bestOrder )
      matrix(index)(index+1) +=
        selectedOrder -> new ViterbiLex( None, selectedOrder, bestOrderScore )

      // Now store the best immediate seals in each direction (which will be determined by the best order)
      if( bestOrder == UnsealedLeftFirst ) {
        val sealedLeft = MarkedObservation( w, SealedLeft )
        matrix(index)(index+1) +=
          sealedLeft -> new LeftHeadedVitSynEntry(
            Option( matrix(index)(index+1)(selectedOrder) ),
            None,
            g.p_stop( StopOrNot( w.w, LeftAttachment, true ) , Stop ) +
              matrix( index )( index+1 )( selectedOrder ).score
          )

        val sealedBoth = MarkedObservation( w, Sealed )
        matrix( index )( index+1 ) +=
          sealedBoth -> new ViterbiSealed(
            Option( matrix( index )( index+1 )( sealedLeft ) ),
            g.p_stop( StopOrNot( w.w, RightAttachment, true ) , Stop ) +
              matrix( index )( index+1 )( sealedLeft ).score
          )
      } else {
        val sealedRight = MarkedObservation( w, SealedRight )
        matrix(index)(index+1) +=
          sealedRight -> new RightHeadedVitSynEntry(
            Option( matrix(index)(index+1)(selectedOrder) ),
            None,
            g.p_stop( StopOrNot( w.w, RightAttachment, true ) , Stop ) +
              matrix( index )( index+1 )( selectedOrder ).score
          )

        val sealedBoth = MarkedObservation( w, Sealed )
        matrix( index )( index+1 ) +=
          sealedBoth -> new ViterbiSealed(
            Option( matrix( index )( index+1 )( sealedRight ) ),
            g.p_stop( StopOrNot( w.w, LeftAttachment, true ) , Stop ) +
              matrix( index )( index+1 )( sealedRight ).score
          )
      }

      // matrix(index)(index+1).keySet.foreach{ w =>
      //   println( Span( index,index+1 ) + ": " + w )
      // }
    }

    def synFill( start:Int, end:Int ) {
      val curSpan = Span(start, end)
      ( (start+1) to (end-1) ).foreach{ k =>
        val rightArgs = matrix( k )( end ).keySet.filter{ _.mark == Sealed }
        val leftArgs = matrix( start )( k ).keySet.filter{ _.mark == Sealed }

        // gather each possible un-sealed rightward-looking head.
        val unsealedLeftHeads = matrix( start )( k ).keySet.filter{ _.mark == UnsealedRightFirst }
        unsealedLeftHeads.foreach{ h =>
          // store the best way to get h from start to k as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.foldLeft(
              Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ), NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              //println( (start,k,end) + ": " + h + " --> " + arg + ": " + newScore )
              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          //println( "++" + (start,k,end) + ": " + h + " --> " + bestArg + ": " + bestArgScore )
          matrix(start)(end) +=
            h -> new LeftHeadedVitSynEntry(
              Option( matrix(start)(k)(h) ),
              Option( matrix(k)(end)(bestArg) ),
              bestArgScore
            )

          // Also, store the left and right seal of this head
          val sealedRight = MarkedObservation( h.obs, SealedRight )
          val sealedRightScore =
            bestArgScore + 
              g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ) , Stop)
          matrix(start)(end) +=
            sealedRight -> new RightHeadedVitSynEntry(
              Option( matrix( start )( end )( h ) ),
              None,
              sealedRightScore
            )

          val sealedBoth = sealedRight.seal.get
          matrix(start)(end) +=
            sealedBoth -> new ViterbiSealed(
              Option( matrix( start )( end )( sealedRight ) ),
              sealedRightScore +
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( sealedRight, Span(start,k) ) ) , Stop)
            )
        }


        // now gather each possible un-sealed leftward-looking head.
        val unsealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>
          // store the best way to get h from start to k as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            leftArgs.foldLeft(
              Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ) , NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , arg.obs.w ) +
                matrix(start)(k)(arg).score +
                matrix(k)(end)(h).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          //println( "Looking for " + bestArg + " in span " + (start,k) )
          matrix(start)(end) +=
            h -> new RightHeadedVitSynEntry(
              Option( matrix(k)(end)(h) ),
              Option( matrix(start)(k)(bestArg) ),
              bestArgScore
            )
          // Also, store the left and right seal of this head
          val sealedLeft = MarkedObservation( h.obs, SealedLeft )
          val sealedLeftScore = bestArgScore +
            g.p_stop( StopOrNot(h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ),Stop)
          matrix(start)(end) +=
            sealedLeft -> new RightHeadedVitSynEntry(
              Option( matrix( start )( end )( h ) ),
              None,
              sealedLeftScore
            )

          val sealedBoth = sealedLeft.seal.get
          matrix(start)(end) +=
            sealedBoth -> new ViterbiSealed(
              Option( matrix(start)(end)(sealedLeft) ),
              sealedLeftScore +
                g.p_stop(StopOrNot(h.obs.w, RightAttachment, adj(sealedLeft, Span(k,end)) ), Stop)
            )
        }


        // gather each possible halfsealed rightward-looking head.
        val halfSealedLeftHeads = matrix( start )( k ).keySet.filter{ _.mark == SealedLeft }
        halfSealedLeftHeads.foreach{ h =>
          // store the best way to get h from start to k as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.foldLeft(
              Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,k) ) ) , NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          matrix(start)(end) +=
            h -> new LeftHeadedVitSynEntry(
              Option( matrix(start)(k)(h) ),
              Option( matrix(k)(end)(bestArg) ),
              bestArgScore
            )

          // Also, store the full seal of this head
          matrix(start)(end) +=
            h.seal.get -> new ViterbiSealed(
              Option( matrix(start)(end)(h) ),
              bestArgScore +
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, Span(start,end) )), Stop )
            )


        }

        // gather each possible halfsealed rightward-looking head.
        val halfSealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == SealedRight }
        halfSealedRightHeads.foreach{ h =>
          // store the best way to get h from k to end as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            leftArgs.foldLeft(
              Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              //println( "Looking for " + arg + " in span " + (start,k) )
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(k,end) ) ) , NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , arg.obs.w ) +
                matrix(k)(end)(h).score +
                matrix(start)(k)(arg).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          matrix(start)(end) +=
            h -> new RightHeadedVitSynEntry(
              Option( matrix(k)(end)(h) ),
              Option( matrix(start)(k)(bestArg) ),
              bestArgScore
            )
          // Also, store the full seal of this head
          matrix(start)(end) +=
            h.seal.get -> new ViterbiSealed(
              Option( matrix(start)(end)(h) ),
              bestArgScore +
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, Span(start,end) )), Stop )
            )
        }


            // // OK, now add seals
            // halfSealedLeftHeads.foreach{ h=>
            //   val sealLeftBasisScore =
            //     matrix(start)(end)(h).score +
            //       g.p_stop( StopOrNot( hObs.w, RightAttachment, adj( sealedLeft, curSpan ) ) ,Stop)

            //   val Tuple2( bestSealBasis, bestSealedScore ) =
            //     if( sealLeftBasisScore > sealRightBasisScore )
            //       Tuple2( SealedLeft, sealLeftBasisScore )
            //     else if( sealLeftBasisScore < sealRightBasisScore )
            //       Tuple2( SealedRight, sealRightBasisScore )
            //     else
            //       if( r.nextDouble > 0.5 )
            //         Tuple2( SealedLeft, sealLeftBasisScore )
            //       else
            //         Tuple2( SealedRight, sealLeftBasisScore )

            //   matrix(start)(end) +=
            //     MarkedObservation( hObs, Sealed ) -> new ViterbiSealed(
            //       Option( matrix(start)(end)( MarkedObservation( hObs, bestSealBasis ) ) ),
            //       bestSealedScore
            //     )
            // }

            // halfSealedLeftHeads.map{ _.obs }.foreach{ hObs =>
            //   val sealedLeft = MarkedObservation(hObs,SealedLeft)
            //   val sealLeftBasisScore =
            //     matrix(start)(end)(sealedLeft).score +
            //       g.p_stop( StopOrNot( hObs.w, RightAttachment, adj( sealedLeft.peel.head, curSpan ) ) ,Stop)

            //   val sealedRight = MarkedObservation(hObs,SealedRight)
            //   val sealRightBasisScore =
            //     matrix(start)(end)(sealedRight).score +
            //       g.p_stop( StopOrNot( hObs.w, LeftAttachment, adj( sealedRight.peel.head, curSpan ) ) ,Stop)


            //   val Tuple2( bestSealBasis, bestSealedScore ) =
            //     if( sealLeftBasisScore > sealRightBasisScore )
            //       Tuple2( SealedLeft, sealLeftBasisScore )
            //     else if( sealLeftBasisScore < sealRightBasisScore )
            //       Tuple2( SealedRight, sealRightBasisScore )
            //     else
            //       if( r.nextDouble > 0.5 )
            //         Tuple2( SealedLeft, sealLeftBasisScore )
            //       else
            //         Tuple2( SealedRight, sealLeftBasisScore )

            //   matrix(start)(end) +=
            //     MarkedObservation( hObs, Sealed ) -> new ViterbiSealed(
            //       Option( matrix(start)(end)( MarkedObservation( hObs, bestSealBasis ) ) ),
            //       bestSealedScore
            //     )
            // }
      }
    }

    def size = s.size

    def toConstituencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length-1 ) , Sealed ) ).constituencyParse

    def toDependencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length-1 ) , Sealed ) ).dependencyParse
  }

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

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
      id + " " + chart.toDependencyParse.mkString( "", ",", "" )
    }
}
