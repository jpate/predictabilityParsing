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
    //println( s.mkString( "<[ ", ", ", " ]>" ) )
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
          (label.peel.toSet & matrix(span.start)(span.end).keySet).map( h =>
            g.p_stop( StopOrNot( label.obs.w, h.attachmentDirection, adj( h, span ) ), Stop ) +
              matrix(span.start)(span.end)( h ).iScore
          ).foldLeft(Double.NegativeInfinity)( Math.sumLogProb(_,_) )
      }
      var oScore = Double.NegativeInfinity
      var score = Double.NegativeInfinity
      def computeMarginal( treeScore:Double ) { score = oScore + iScore - treeScore }

      def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
      def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

      def setOScore( x:Double ) { oScore = x }


      def addDependency( headEntry:Entry, argEntry:Entry ) {
        //dependents += argEntry
        val h = headEntry.label
        assert( label == h )
        val a = argEntry.label
        incrementIScore(
          g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, span ) ) , NotStop ) +
          g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
          argEntry.iScore +
          headEntry.iScore
        )
      }
    }

    class TerminalEntry( h:MarkedObservation, index:Int ) extends Entry( h, Span( index, index +1 ) )


    def treeScore =
      matrix( 0 )( s.length )( MarkedObservation( FinalRoot( s.length-1 ), Sealed ) ).iScore


        // def lexFillOld( index:Int ) {
        //   val w = s(index)

        //   val leftFirst = MarkedObservation( w, UnsealedLeftFirst )
        //   // if( w.w == Root) println( "\t\tAdding " + leftFirst + " to " + (index,index+1) )
        //   matrix( index )( index+1 ) += leftFirst -> new LexEntry( leftFirst, Nil )
        //   matrix( index )( index+1 )( leftFirst ).setIScore( g.p_order( w.w , LeftFirst ) )


        //   val rightFirst = MarkedObservation( w, UnsealedRightFirst )
        //   // if( w.w == Root) println( "\t\tAdding " + rightFirst + " to " + (index,index+1) )
        //   matrix( index )( index+1 ) +=
        //     rightFirst -> new LexEntry( rightFirst, Nil )
        //   matrix( index )( index+1 )( rightFirst ).setIScore( g.p_order(w.w, RightFirst) )


        //   // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
        //   // between i and j
        //   val sealedLeft = MarkedObservation( w, SealedLeft )
        //   // if( w.w == Root) println( "\t\tAdding " + sealedLeft + " to " + (index,index+1) )
        //   matrix( index )( index+1 ) +=
        //     sealedLeft -> new LexEntry( sealedLeft, List( matrix(index)(index+1)(leftFirst) ) )
        //   matrix( index )( index+1 )( sealedLeft ).setIScore(
        //     g.p_stop( StopOrNot( w.w, LeftAttachment, true ) , Stop ) +
        //     matrix( index )( index+1 )( leftFirst ).iScore
        //   )



        //   // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
        //   // between i and j
        //   val sealedRight = MarkedObservation( w, SealedRight )
        //   // if( w.w == Root) println( "\t\tAdding " + sealedRight + " to " + (index,index+1) )
        //   matrix( index )( index+1 ) +=
        //     sealedRight -> new LexEntry( sealedRight, List( matrix(index)(index+1)(rightFirst) ) )
        //   matrix( index )( index+1 )( sealedRight ).setIScore(
        //     g.p_stop( StopOrNot( w.w, RightAttachment, true ) , Stop ) +
        //     matrix( index )( index+1 )( rightFirst ).iScore
        //   )



        //   val sealedBoth = MarkedObservation( w, Sealed )
        //   // if( w.w == Root) println( "\t\tAdding " + sealedBoth + " to " + (index,index+1) )
        //   matrix( index )( index+1 ) +=
        //     sealedBoth -> new LexEntry(
        //         sealedBoth,
        //         List(
        //           matrix(index)(index+1)(sealedLeft),
        //           matrix(index)(index+1)(sealedRight)
        //         )
        //       )
        //   matrix( index )( index+1 )( sealedBoth ).setIScore(
        //     Math.sumLogProb( 
        //       g.p_stop( StopOrNot( w.w, LeftAttachment, true ) , Stop ) +
        //         matrix( index )( index+1 )( sealedRight ).iScore ,
        //       g.p_stop( StopOrNot( w.w, RightAttachment, true ) , Stop ) +
        //         matrix( index )( index+1 )( sealedLeft ).iScore
        //     )
        //   )
        // }

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

      // println( matrix(0)(n).keySet )
      matrix( 0 )( n )( MarkedObservation( FinalRoot(n-1), Sealed ) ).setOScore( 0D )
      // 1 to (n) rather than 1 to (n-1) because we do have unary branches over the whole sentence
      ( 1 to n ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>
          val j = i + length

          // Compute from most-sealed to least-sealed.
          // So Sealed first.
          val curSpanSealedNodes = matrix(i)(j).keySet.filter{ _.mark == Sealed }
          curSpanSealedNodes.foreach{ a =>
            // Look both left and right for each sealed node. First, look left.
            // to (i-1) so we don't bother looking for possible heads in spans of length 0
            (0 to (i-1) ).foreach{ k =>
              // Gather half-sealed right-looking heads:
              val halfSealedRightwardHeads = matrix(k)(i).keySet.filter{ _.mark == SealedLeft }

              // Gather unsealed right-looking heads:
              val unsealedRightwardHeads = matrix(k)(i).keySet.filter{ _.mark == UnsealedRightFirst }

              ( halfSealedRightwardHeads ++ unsealedRightwardHeads ).foreach{ h =>
                matrix(i)(j)(a).incrementOScore(
                  g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj(h.obs,i) ) , NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                  matrix( k )( i )( h ).iScore +
                  matrix( k )( j )( h ).oScore
                )
              }

            }


            // Now look right. Similarly, from j+1 so we don't bother looking for possible heads
            // in spans of length 1
            ( (j+1) to n ).foreach{ k =>
              // Gather half-sealed left-looking heads:
              val halfSealedLeftwardHeads = matrix(j)(k).keySet.filter{ _.mark == SealedRight }

              // Gather unsealed left-looking heads:
              val unsealedLeftwardHeads = matrix(j)(k).keySet.filter{ _.mark == UnsealedLeftFirst }

              ( halfSealedLeftwardHeads ++ unsealedLeftwardHeads ).foreach{ h =>
                matrix(i)(j)(a).incrementOScore(
                  g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj(h.obs,j) ) , NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                  matrix( j )( k )( h ).iScore +
                  matrix( i )( k )( h ).oScore
                )
              }

            }
          }

          // First, half-sealed to the left
          val halfSealedLeft = matrix(i)(j).keySet.filter{ _.mark == SealedLeft }
          halfSealedLeft.foreach{ w =>
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, RightAttachment , adj( w, j ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, j ) ), NotStop ) +
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
              g.p_stop(StopOrNot( w.obs.w, LeftAttachment , adj( w, Span(i,j) ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ), NotStop ) +
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
              g.p_stop( StopOrNot( w.obs.w, LeftAttachment , adj( w, Span(i,j) ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( 0 to (i-1) ).foreach{ k =>
              val leftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              leftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ), NotStop ) +
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
              g.p_stop( StopOrNot( w.obs.w, RightAttachment , adj( w, j ) ) , Stop ) +
                matrix(i)(j)(w.seal.get).oScore
            )

            ( (j+1) to n ).foreach{ k =>
              val rightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              rightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, j ) ), NotStop ) +
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

      // First, produces marginals from inside and outside scores for every entry.
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          matrix(i)(j).values.foreach( _.computeMarginal( treeScore ) )
        }
      }

      // val opportunitiesToStop = HashMap[StopOrNot,Double]
      // val successfulStops = HashMap[StopOrNot,HashMap[StopDecision,Double]]

      ( 0 to (s.length-1 ) ).foreach{ i =>
        ( (i+1) to (s.length-1) ).foreach{ j =>
          val curSpan = Span(i,j)
          // increment opportunitiesToStop
          matrix(i)(j).keySet.filterNot{ _.seal.isEmpty }.foreach{ h =>
            pc.incrementStopDenomCounts(
              StopOrNot( h.obs.w, h.attachmentDirection, adj( h, curSpan) ),
              matrix(i)(j)(h).score
            )
          }
          matrix(i)(j).keySet.filter{ _.peel.length > 0 }.foreach{ h =>
            h.peel.foreach{ peeledH =>
              pc.incrementStopDenomCounts(
                StopOrNot( h.obs.w, peeledH.attachmentDirection, adj( peeledH, curSpan) ),
                matrix(i)(j)(h).score
              )
            }
          }
        }
      }


      (0 to (s.length-1) ).foreach{ i =>
        // count up order preference:

        pc.incrementOrderCounts(
          s(i),
          RightFirst,
          matrix(i)(i+1)(MarkedObservation( s(i), UnsealedRightFirst ) ).score
        )
        pc.incrementOrderCounts(
          s(i),
          LeftFirst,
          matrix(i)(i+1)(MarkedObservation( s(i), UnsealedLeftFirst ) ).score
        )

        ( (i+1) to s.length ).foreach{ j =>
          val curSpan = Span( i,j)
          // count everything up. We'll do the division through one normalization step at the end.

          // First, increment stop counts. We will compute the final p_stop in the following
          // way. Keep track of the numerator and denominator of p_stop. Then, in toDMVGrammar of
          // DMVPartialCounts, divide to obtain p_stop( ., Stop ). Then set p_stop( ., NotStop ) =
          // 1- p_stop( ., Stop ) Remember that p_stop is a CPT, so p_stop( x, y) is, in more usual
          // notation, P_stop( y | x )
          // matrix(i)(j).keySet.filter{_.mark == Sealed}.foreach{ h =>
          //   val eachHalfSealed = h.peel

          //   eachHalfSealed.foreach{ halfSealed =>
          //     val unSealed = halfSealed.peel.head

          //     pc.incrementStopCounts(
          //       StopOrNot( h.obs.w, halfSealed.attachmentDirection, adj( halfSealed, curSpan ) ),
          //       Stop,
          //       matrix(i)(j)(h).score
          //     )
          //     pc.incrementStopDenomCounts(
          //       StopOrNot( h.obs.w, halfSealed.attachmentDirection, adj( halfSealed, curSpan ) ),
          //       matrix(i)(j)(halfSealed).score
          //     )

          //     pc.incrementStopCounts(
          //       StopOrNot( h.obs.w, unSealed.attachmentDirection, adj( unSealed, curSpan ) ),
          //       Stop,
          //       matrix(i)(j)(halfSealed).score
          //     )
          //     println( "Looking for " + unSealed + " in span " + Span(i,j) )
          //     pc.incrementStopDenomCounts(
          //       StopOrNot( h.obs.w, unSealed.attachmentDirection, adj( unSealed, curSpan ) ),
          //       matrix(i)(j)(unSealed).score
          //     )
          //   }
          // }

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
                  g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h, k ) ) , NotStop ) +
                    g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) , a.obs.w ) +
                      matrix(i)(k)(h).score
                )
              }
            }

            val leftArguments = matrix(i)(k).keySet.filter{ _.mark == Sealed }
            matrix(k)(j).keySet.filter{ _.attachmentDirection == LeftAttachment }.foreach{ h =>
              leftArguments.foreach{ a =>
                pc.incrementChooseCounts(
                  ChooseArgument( h.obs.w, LeftAttachment ),
                  a.obs.w,
                  g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h, k ) ) , NotStop ) +
                    g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , a.obs.w ) +
                      matrix(k)(j)(h).score
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
  private val r = new Random()

  val g = new DMVGrammar( vocabulary = Set[ObservedLabel]() )

  class ViterbiChart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,AbstractVitEntry]()
    )


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
        Set( DirectedArc( h.get.headLabel.obs, a.get.headLabel.obs ) ) ++ a.get.dependencyParse
    }

    class LeftHeadedVitSynEntry(
      h:Option[AbstractVitEntry],
      a:Option[AbstractVitEntry],
      score:Double
    ) extends AbstractVitSynEntry( h, RightAttachment, a, score ) {
      def constituencyParse =
        "(" + h.get.headLabel + " " + h.get.constituencyParse + " " + a.get.constituencyParse + " )"
    }
    class RightHeadedVitSynEntry(
      h:Option[AbstractVitEntry],
      a:Option[AbstractVitEntry],
      score:Double
    ) extends AbstractVitSynEntry( h, LeftAttachment, a, score ) {
      def constituencyParse =
        "(" + h.get.headLabel +
          { if( a.isEmpty ) "" else  " " + a.get.constituencyParse } +
            " " + h.get.constituencyParse + " )"
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
          sealedLeft -> new RightHeadedVitSynEntry(
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
          sealedRight -> new LeftHeadedVitSynEntry(
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
    }

    def synFill( start:Int, end:Int ) {
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
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) , NotStop ) +
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
          // Also, store the right seal of this head
          val sealedRight = MarkedObservation( h.obs, SealedRight )
          matrix(start)(end) +=
            sealedRight -> new RightHeadedVitSynEntry(
              Option( matrix( start )( end )( h ) ),
              None,
              bestArgScore + g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) , Stop)
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
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h.obs, k ) ) , NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) , arg.obs.w ) +
                matrix(start)(k)(arg).score +
                matrix(k)(end)(h).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            }

          matrix(start)(end) +=
            h -> new LeftHeadedVitSynEntry(
              Option( matrix(k)(end)(h) ),
              Option( matrix(start)(k)(bestArg) ),
              bestArgScore
            )
          // Also, store the left seal of this head
          val sealedLeft = MarkedObservation( h.obs, SealedLeft )
          matrix(start)(end) +=
            sealedLeft -> new RightHeadedVitSynEntry(
              Option( matrix( start )( end )( h ) ),
              None,
              bestArgScore + g.p_stop( StopOrNot(h.obs.w, RightAttachment, adj( h.obs, k ) ),Stop)
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
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) , NotStop ) +
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

          // DO NOT store the right seal of this head; we need to decide if we want to have a Sealed
          // head from SealedRight or from SealedLeft
                // Also, store the right seal of this head
                // val sealedRight = MarkedObservation( h.obs, SealedRight )
                // matrix(start)(end) +=
                //   sealedRight -> new RightHeadedVitSynEntry(
                //     Option( matrix( start )( end )( sealedRight ) ),
                //     None,
                //     bestArgScore + g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )
                //   )
        }

        // gather each possible halfsealed rightward-looking head.
        val halfSealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == SealedRight }
        halfSealedRightHeads.foreach{ h =>
          // store the best way to get h from k to end as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.foldLeft(
              Tuple2[MarkedObservation,Double]( null, Double.NegativeInfinity )
            ){ (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h.obs, k ) ) , NotStop ) +
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
          // DO NOT store the right seal of this head; we need to decide if we want to have a Sealed
          // head from SealedRight or from SealedLeft
        }


        assert( halfSealedLeftHeads.map{_.obs} == halfSealedRightHeads.map{_.obs} )
        // OK, now decide whether we want a Sealed node from having sealed SealedLeft or SealedRight
        // for each half sealed head
        halfSealedLeftHeads.map{ _.obs }.foreach{ hObs =>
          val sealLeftBasisScore =
            matrix(start)(end)(MarkedObservation(hObs,SealedLeft)).score +
              g.p_stop( StopOrNot( hObs.w, RightAttachment, adj( hObs, end ) ) ,Stop)
          val sealRightBasisScore =
            matrix(start)(end)(MarkedObservation(hObs,SealedRight)).score +
              g.p_stop( StopOrNot( hObs.w, LeftAttachment, adj( hObs, start ) ) ,Stop)


          val Tuple2( bestSealBasis, bestSealedScore ) =
            if( sealLeftBasisScore > sealRightBasisScore )
              Tuple2( SealedLeft, sealLeftBasisScore )
            else if( sealLeftBasisScore < sealRightBasisScore )
              Tuple2( SealedRight, sealRightBasisScore )
            else
              if( r.nextDouble > 0.5 )
                Tuple2( SealedLeft, sealLeftBasisScore )
              else
                Tuple2( SealedRight, sealLeftBasisScore )

          matrix(start)(end) +=
            MarkedObservation( hObs, Sealed ) -> new ViterbiSealed(
              Option( matrix(start)(end)( MarkedObservation( hObs, bestSealBasis ) ) ),
              bestSealedScore
            )
        }



      }
    }

    def toConstituencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length ) , Sealed ) ).constituencyParse

    def toDependencyParse =
      matrix(0)(s.length)( MarkedObservation( FinalRoot( s.length ) , Sealed ) ).dependencyParse
  }

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

  def populateChart( s:List[TimedObservedLabel] ) = {
    val chart =
      if( s.last != FinalRoot( s.length ) )
        new ViterbiChart( s :+ FinalRoot( s.length ) )
      else
        new ViterbiChart( s )

    (1 to ( s.size )) foreach{ j =>
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
