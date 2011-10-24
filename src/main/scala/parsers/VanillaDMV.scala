package predictabilityParsing.parsers

import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log


class VanillaDMVEstimator {
  val g = new DMVGrammar( vocabulary = Set[Word]() )

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }


  /*
   * A class for charts to populate. For now, we'll only allow one constituent category, especially
   * since Klein found multiple categories hurt performance.
   */
  class Chart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,AbstractEntry]()
    )

    //class Entry( head:ObservedLabel, val attStatus:AttachmentStatus ) {
    abstract class AbstractEntry( val head:MarkedObservation, val start:Int, val end:Int ) {
      var iScore = Double.NegativeInfinity
      var oScore = Double.NegativeInfinity
      var score = 0D // Set this after setting iScore and oScore
      var dependents:Set[AbstractEntry] = Set[AbstractEntry]()

      def setIScore( updatedScore:Double ) { iScore = updatedScore }
      def setOScore( updatedScore:Double ) { oScore = updatedScore }
      def setScore( newScore:Double ) { score = newScore }
      def computeMarginal( treeScore:Double ) { score = oScore + iScore / treeScore }

      def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
      def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

      def addDependency( headEntry:AbstractEntry, argEntry:AbstractEntry ):Unit

      override def toString = head.toString
    }

    class LexEntry( markedObs:MarkedObservation )
      extends AbstractEntry( markedObs, markedObs.obs.t, markedObs.obs.t+1 ) {
      def addDependency( headEntry:AbstractEntry, argEntry:AbstractEntry ) = ()
    }

    class SealedSynEntry( h:MarkedObservation, i:Int, j:Int ) extends AbstractEntry( h, i:Int, j:Int ) {
      def addDependency( headEntry:AbstractEntry, argEntry:AbstractEntry ) = ()
    }

    // TODO consider removing attachDir
    abstract class SynEntry( h:MarkedObservation, val attachDir:AttachmentDirection, i:Int, j:Int )
      extends AbstractEntry( h, i, j ) 

    class LeftHeadedSynEntry( h:MarkedObservation, i:Int, j:Int )
      extends SynEntry( h, RightAttachment, i, j ) {
      def addDependency( headEntry:AbstractEntry, argEntry:AbstractEntry ) {
        assert( headEntry.head == h )
        dependents += argEntry
        incrementIScore(
          g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, argEntry.start ) ) )( NotStop ) +
          g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) )( argEntry.head.obs ) +
          argEntry.iScore +
          headEntry.iScore
        )
      }
    }

    class RightHeadedSynEntry( h:MarkedObservation, i:Int, j:Int )
      extends SynEntry( h, LeftAttachment, i, j ) {
      def addDependency( headEntry:AbstractEntry, argEntry:AbstractEntry ) {
        assert( headEntry.head == h )
        dependents += argEntry
        incrementIScore(
          g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h.obs, argEntry.end ) ) )( NotStop ) +
          g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) )( argEntry.head.obs ) +
          argEntry.iScore +
          headEntry.iScore
        )
      }
    }

    def treeScore =
      matrix( 0 )( s.length )( MarkedObservation( FinalRoot( s.length ), Sealed ) ).iScore

    def lexFill( index:Int ) {
      val w = s(index)

      val leftFirst = MarkedObservation( w, UnsealedLeftFirst )
      matrix( index )( index+1 ) += leftFirst -> new LexEntry( leftFirst )
      matrix( index )( index+1 )( leftFirst ).setIScore( g.p_order( w.w )( LeftFirst ) )


      val rightFirst = MarkedObservation( w, UnsealedRightFirst )
      matrix( index )( index+1 ) +=
        rightFirst -> new LexEntry( rightFirst )
      matrix( index )( index+1 )( rightFirst ).setIScore( g.p_order(w.w)(RightFirst) )


      // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
      // between i and j
      val sealedLeft = MarkedObservation( w, SealedLeft )
      matrix( index )( index+1 ) +=
        sealedLeft -> new LexEntry( sealedLeft )
      matrix( index )( index+1 )( sealedLeft ).setIScore(
        g.p_stop( StopOrNot( w.w, LeftAttachment, true ) )( Stop ) +
        matrix( index )( index+1 )( leftFirst ).iScore
      )



      // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
      // between i and j
      val sealedRight = MarkedObservation( w, SealedRight )
      matrix( index )( index+1 ) +=
        sealedRight -> new LexEntry( sealedRight )
      matrix( index )( index+1 )( sealedRight ).setIScore(
        g.p_stop( StopOrNot( w.w, RightAttachment, true ) )( Stop ) +
        matrix( index )( index+1 )( rightFirst ).iScore
      )



      val sealedBoth = MarkedObservation( w, Sealed )
      matrix( index )( index+1 ) +=
        sealedBoth -> new LexEntry( sealedBoth )
      matrix( index )( index+1 )( sealedBoth ).setIScore(
        Math.sumLogProb( 
          g.p_stop( StopOrNot( w.w, LeftAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( sealedRight ).iScore ,
          g.p_stop( StopOrNot( w.w, RightAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( sealedLeft ).iScore
        )
      )
    }

    def synFill( start:Int, end:Int ) {
      // no need to select attachment direction order; that occurs in span of length 1.
      // Let's do least-sealed first to most-sealed last so that we can easily guarantee that the
      // children of unary rules have already been computed.

      // First, unsealed scores for spans larger than one (top of Klein's thesis p. 108).
      ( (start+1) to (end-1) ).foreach{ k =>
        // Since we have the same possible left and right arguments for the same k, go ahead and
        // gather them now.
        val rightArgs = matrix( k )( end ).keySet.filter{ _.mark == Sealed }
        val leftArgs = matrix( start )( k ).keySet.filter{ _.mark == Sealed }



        // Left-headed phrases first
        // find possible unsealed left heads
        val unsealedLeftHeads = matrix( start )( k ).keySet.filter{ _.mark == UnsealedRightFirst }

        unsealedLeftHeads.foreach{ h =>
          if( !( matrix( start )( end ).contains( h ) ) )
            matrix( start )( end ) += h -> new LeftHeadedSynEntry( h, start, end )

          rightArgs.foreach{ rightArg =>
            matrix( start )( end )( h ).addDependency(
              matrix( start )( k )( h ),
              matrix( k )( end )( rightArg )
            )
          }
        }

        // Ok, now right-headed phrases
        // find possible unsealed right heads
        val unsealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == UnsealedLeftFirst }

        unsealedRightHeads.foreach{ h =>
          if( !( matrix( start )( end ).contains( h ) ) )
            matrix( start )( end ) += h -> new RightHeadedSynEntry( h, start, end )

          leftArgs.foreach{ leftArg =>
            matrix( start )( end )( h ).addDependency(
              matrix( k )( end )( h ),
              matrix( start )( k )( leftArg )
            )
          }

        }
      }


      // Good, now let's do half-sealed heads for the current span.
      // Only do the summations insead the k foreach loop, increment by inside score of unary child
      // and stop after the loop. So the k loop will actually be identical to the above k loop for unsealed
      // parents (except for filtering out half-sealed left heads rather than unsealed left heads),
      // but afterwards we also increment by the inside score of unary child and stop
      ( (start+1) to (end-1) ).foreach{ k =>
        val rightArgs = matrix( k )( end ).keySet.filter{ _.mark == Sealed }
        val leftArgs = matrix( start )( k ).keySet.filter{ _.mark == Sealed }

        // Left-heads first:

        val halfSealedLeftHeads = matrix( start )( k ).keySet.filter{ _.mark == SealedLeft }

        halfSealedLeftHeads.foreach{ h =>
          if( !( matrix( start )( end ).contains( h ) ) )
            matrix( start )( end ) += h -> new LeftHeadedSynEntry( h, start, end )

          rightArgs.foreach{ rightArg =>
            matrix( start )( end )( h ).addDependency(
              matrix( start )( k )( h ),
              matrix( k )( end )( rightArg )
            )
          }
        }

        // Ok, now right-heads

        val halfSealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == SealedRight }

        halfSealedRightHeads.foreach{ h =>
          if( !( matrix( start )( end ).keySet.contains( h ) ) )
            matrix( start )( end ) += h -> new RightHeadedSynEntry( h, start, end )

          leftArgs.foreach{ leftArg =>
            matrix( start )( end )( h ).addDependency(
              matrix( k )( end )( h ),
              matrix( start )( k )( leftArg )
            )
          }

        }
      }

      // now increment by probability of sealing an unsealed left-first head to the left
      val fullSpanHalfSealedLeftHeads = matrix( start )( end ).keySet.filter{ _.mark == SealedLeft }
      fullSpanHalfSealedLeftHeads.foreach{ h =>
        matrix( start )( end )( h ).incrementIScore(
          g.p_stop( StopOrNot( h.obs, LeftAttachment, adj( h.obs, start ) ) )( Stop ) +
          matrix( start )( end )( MarkedObservation( h.obs, UnsealedLeftFirst ) ).iScore
        )
      }

      // now increment by probability of sealing an unsealed right-first head to the to the right
      val fullSpanHalfSealedRightHeads = matrix( start )( end ).keySet.filter{ _.mark == SealedRight }
      fullSpanHalfSealedRightHeads.foreach{ h =>
        matrix( start )( end )( h ).incrementIScore(
          g.p_stop( StopOrNot( h.obs, RightAttachment, adj( h.obs, end ) ) )( Stop ) +
          matrix( start )( end )( MarkedObservation( h.obs, UnsealedRightFirst ) ).iScore
        )
      }



      assert( fullSpanHalfSealedLeftHeads.map{_.obs} == fullSpanHalfSealedRightHeads.map{_.obs} )
      // Finally, we do sealed heads for current span (very top of p. 107)
      fullSpanHalfSealedLeftHeads.map{_.obs}.foreach{ hObs =>
        val h = MarkedObservation( hObs, Sealed )
        val sealedLeft = MarkedObservation( hObs, SealedLeft )
        val sealedRight = MarkedObservation( hObs, SealedRight )
        matrix( start )( end ) += h -> new SealedSynEntry( h, start, end )
        matrix( start )( end )( h ).dependents ++=
          Set( matrix( start )( end )( sealedLeft ) , matrix( start )( end)( sealedRight ) )

        matrix( start )( end )( h ).incrementIScore(
          Math.sumLogProb(
            g.p_stop( StopOrNot( hObs, LeftAttachment, adj( hObs, start ) ) )( Stop ) +
              matrix( start )( end )( sealedRight ).iScore,
            g.p_stop( StopOrNot( hObs, RightAttachment, adj( hObs, end ) ) )( Stop ) +
              matrix( start )( end )( sealedLeft ).iScore
          )
        )
      }
    }

    private def adj( w1:TimedObservedLabel, w2:TimedObservedLabel ) = math.abs( w1.t - w2.t ) <= 1
    private def adj( w1:MarkedObservation, w2:MarkedObservation ) = math.abs( w1.obs.t - w2.obs.t ) <= 1
    private def adj( w1:TimedObservedLabel, w2:Int ) = math.abs( w1.t - w2 ) <= 1
    private def adj( w1:MarkedObservation, w2:Int ) = math.abs( w1.obs.t - w2 ) <= 1

    def outsidePass {
      import math.exp
      val n = s.length

      matrix( 0 )( n )( MarkedObservation( FinalRoot(n-1), Sealed ) ).setOScore( 0D )
      // 1 to (n) rather than 1 to (n-1) because we do have unary branches over the whole sentence
      ( 1 to n ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>
          val j = i + length


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
                  g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj(h.obs,i) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) )( a.obs.w ) +
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
                  g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj(h.obs,j) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) )( a.obs.w ) +
                  matrix( j )( k )( h ).iScore +
                  matrix( i )( k )( h ).oScore
                )
              }

            }
          }

          // Ok, now do outside scores for all unary branches, working from most-sealed to
          // least-sealed.

          // Half-sealed leftward looking first
          val curSpanSealedRight = matrix( i )( j ).keySet.filter{ _.mark == SealedRight }
          curSpanSealedRight.foreach{ w =>
            // possibility we are the immediate child of a stop rule.
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ) )( Stop ) +
              matrix(i)(j)( MarkedObservation( w.obs, Sealed ) ).oScore
            )

            // possiblity we are the (right) head of a left attachment rule. Sum over all possible
            // left attachments. to (i-1) for the same reason as above
            (0 to (i-1)).foreach{ k =>
              val possibleLeftArguments = matrix(k)(i).keySet.filter{ _.mark == Sealed }
              possibleLeftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( w.obs.w, LeftAttachment ) )( a.obs.w ) +
                  matrix(k)(i)(a).iScore +
                  matrix(k)(j)(w).oScore
                )
              }
            }
          }

          // Half-sealed rightward looking first
          val curSpanSealedLeft = matrix( i )( j ).keySet.filter{ _.mark == SealedLeft }
          curSpanSealedLeft.foreach{ w =>
            // possibility we are the immediate child of a stop rule.
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, j ) ) )( Stop ) +
              matrix(i)(j)( MarkedObservation( w.obs, Sealed ) ).oScore
            )

            // possiblity we are the (left) head of a right attachment rule. Sum over all possible
            // right attachments. from (j+1) for the same reason as above
            ((j+1) to n).foreach{ k =>
              val possibleRightArguments = matrix(j)(k).keySet.filter{ _.mark == Sealed }
              possibleRightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, j ) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( w.obs.w, RightAttachment ) )( a.obs.w ) +
                  matrix(j)(k)(a).iScore +
                  matrix(i)(k)(w).oScore
                )
              }
            }
          }

          // Good. now let's do unsealed heads. Left-first heads first.
          val curSpanUnsealedLeftFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedLeftFirst }
          curSpanUnsealedLeftFirst.foreach{ w =>
            // possibility we are the immediate child of a stop rule.
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ) )( Stop ) +
              matrix(i)(j)( MarkedObservation( w.obs, SealedLeft ) ).oScore
            )

            // possiblity we are the (right) head of a left attachment rule. Sum over all possible
            // left attachments. to (i-1) for the same reason as above
            (0 to (i-1)).foreach{ k =>
              val possibleLeftArguments = matrix(k)(i).keySet.filter( _.mark == Sealed )
              possibleLeftArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, LeftAttachment, adj( w, i ) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( w.obs.w, LeftAttachment ) )( a.obs.w ) +
                  matrix(k)(i)(a).iScore +
                  matrix(k)(j)(w).oScore
                )
              }
            }
          }


          // and now unsealed right-first heads.
          val curSpanUnsealedRightFirst = matrix(i)(j).keySet.filter{ _.mark == UnsealedRightFirst }
          curSpanUnsealedLeftFirst.foreach{ w =>
            // possibility we are the immediate child of a stop rule.
            matrix(i)(j)(w).incrementOScore(
              g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, i ) ) )( Stop ) +
              matrix(i)(j)( MarkedObservation( w.obs, SealedRight ) ).oScore
            )

            // possiblity we are the (right) head of a left attachment rule. Sum over all possible
            // left attachments. from j+1 for the same reason as above
            ((j+1) to n).foreach{ k =>
              val possibleRightArguments = matrix(j)(k).keySet.filter( _.mark == Sealed )
              possibleRightArguments.foreach{ a =>
                matrix(i)(j)(w).incrementOScore(
                  g.p_stop( StopOrNot( w.obs.w, RightAttachment, adj( w, j) ) )( NotStop ) +
                  g.p_choose( ChooseArgument( w.obs.w, RightAttachment ) )( a.obs.w ) +
                  matrix(j)(k)(a).iScore +
                  matrix(i)(k)(w).oScore
                )
              }
            }
          }

          // Tada!!

        }
      )
    }

    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = new DMVPartialCounts //( s.map{ _.obs }.toSet )

      // First, produces marginals from inside and outside scores for every entry.
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          matrix(i)(j).values.foreach( _.computeMarginal( treeScore ) )
        }
      }

      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>

          // increment everything from the partial counts
          // Since order no longer matters, just take care of the different cases with a
          // construction of: match { case ... }

          matrix(i)(j).values.foreach{ entry =>
            val markedHead = entry.head
            val timedHead = markedHead.obs
            val h = timedHead.w
            markedHead.mark match {
              case UnsealedRightFirst => {
                // Gather counts for order preference if span length 1
                if( j - i == 1 )
                  pc.incrementOrderCounts( h, RightFirst, entry.score )

                // Gather counts for right attachments
                entry.dependents.foreach{ argEntry =>
                  pc.incrementChooseCounts(
                    ChooseArgument( h, RightAttachment ),
                    argEntry.head.obs.w,
                    argEntry.score
                  )
                  pc.incrementStopCounts(
                    StopOrNot( h, RightAttachment, adj( timedHead, argEntry.start ) ),
                    NotStop,
                    argEntry.score
                  )
                }
              }
              case UnsealedLeftFirst => {
                // Gather counts for order preference if span length 1
                if( j - i == 1 )
                  pc.incrementOrderCounts( h, LeftFirst, entry.score )

                // Gather counts for left attachments
                entry.dependents.foreach{ argEntry =>
                  pc.incrementChooseCounts(
                    ChooseArgument( h, LeftAttachment ),
                    argEntry.head.obs.w,
                    argEntry.score
                  )
                  pc.incrementStopCounts(
                    StopOrNot( h, LeftAttachment, adj( timedHead, argEntry.start ) ),
                    NotStop,
                    argEntry.score
                  )
                }
              }
              case SealedRight => {
                // Gather counts for left attachments
                entry.dependents.foreach{ argEntry =>
                  pc.incrementChooseCounts(
                    ChooseArgument( h, LeftAttachment ),
                    argEntry.head.obs.w,
                    argEntry.score
                  )
                  pc.incrementStopCounts(
                    StopOrNot( h, LeftAttachment, adj( timedHead, argEntry.start ) ),
                    NotStop,
                    argEntry.score
                  )
                }
                // Gather counts for stopping
                pc.incrementStopCounts(
                  StopOrNot( h, RightAttachment, adj( timedHead, entry.end ) ),
                  Stop,
                  entry.score
                )
              }
              case SealedLeft => {
                // Gather counts for right attachments
                entry.dependents.foreach{ argEntry =>
                  pc.incrementChooseCounts(
                    ChooseArgument( h, RightAttachment ),
                    argEntry.head.obs.w,
                    argEntry.score
                  )
                  pc.incrementStopCounts(
                    StopOrNot( h, RightAttachment, adj( timedHead, argEntry.end ) ),
                    NotStop,
                    argEntry.score
                  )
                }
                // Gather counts for stopping
                pc.incrementStopCounts(
                  StopOrNot( h, LeftAttachment, adj( timedHead, entry.start ) ),
                  Stop,
                  entry.score
                )
              }
              case Sealed => {
                // No attachments, only possibility of having stopped to the left and to the right.
                pc.incrementStopCounts(
                  StopOrNot( h, RightAttachment, adj( timedHead, entry.end ) ),
                  Stop,
                  entry.score
                )
                pc.incrementStopCounts(
                  StopOrNot( h, LeftAttachment, adj( timedHead, entry.start ) ),
                  Stop,
                  entry.score
                )
              }
            }
          }

        }
      }

      pc.setTotalScore( treeScore )

      pc
    }

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
    val chart = new Chart( s )

    (1 to ( s.size )) foreach{ j =>
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
    corpus.par.map{ s => populateChart(s).toPartialCounts }.reduce{(a,b) => a.destructivePlus(b); a}

}

class VanillaDMVParser {
  val g = new DMVGrammar( vocabulary = Set[Word]() )

  class ViterbiChart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )(
      collection.mutable.Map[MarkedObservation,AbstractEntry]()
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
      extends AbstractVitEntry( lexChild, lex, None, score ) {
      def constituencyParse =
        "(" + lexLabel + " " +
          { if( lexChild.isEmpty ) lexLabel.obs else lexChild.get.constituencyParse } + ")"
      def dependencyParse = Set[DirectedArc]()
    }

    class ViterbiSealed( h:Option[AbstractVitSynEntry], score:Double )
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
    ) extends AbstractVitEntry( h, h.headLabel, a, score ) {
      def dependencyParse = Set( DirectedArc( h.get, a.get ) ) ++ a.get.dependencyParse
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
      import scala.util.Random
      val r = new Random()
      val w = s(index)

      // First, pick best order.
      val bestOrder =
        if( g.p_order( w.w )( UnsealedLeftFirst ) > g.p_order( w.w )( UnsealedRightFirst ) )
          UnsealedLeftFirst
        else if( g.p_order( w.w )( UnsealedLeftFirst ) < g.p_order( w.w )( UnsealedRightFirst ) )
          UnsealedRightFirst
        else
          if( r.nextDouble >= 0.5 ) UnsealedLeftFirst else UnsealedRightFirst

      val orderSelection = MarkedObservation( w, bestOrder )
      matrix(index)(index+1) +=
        orderSelection -> new ViterbiLex( orderSelection, g.p_order( w.w )( bestOrder ) )

      // Now store the best immediate seals in each direction (which will be determined by the best order)
      if( bestOrder == UnsealedLeftFirst ) {
        val sealedLeft = MarkedObservation( w, SealedLeft )
        matrix(index)(index+1) +=
          sealedLeft -> new RightHeadedVitSynEntry(
            Option( matrix(index)(index+1)(bestOrder) ),
            None,
            g.p_stop( StopOrNot( w.w, LeftAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( bestOrder ).score
          )

        val sealedBoth = MarkedObservation( w, Sealed )
        matrix( index )( index+1 ) +=
          sealedBoth -> new ViterbiSealed(
            Option( matrix( index )( index+1 )( sealedLeft ) ),
            g.p_stop( StopOrNot( w.w, RightAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( sealedLeft ).score
          )
      } else {
        val sealedRight = MarkedObservation( w, SealedRight )
        matrix(index)(index+1) +=
          sealedRight -> new LeftHeadedVitSynEntry(
            Option( matrix(index)(index+1)(bestOrder) ),
            None,
            g.p_stop( StopOrNot( w.w, RightAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( bestOrder ).score
          )

        val sealedBoth = MarkedObservation( w, Sealed )
        matrix( index )( index+1 ) +=
          sealedBoth -> new ViterbiSealed(
            Option( matrix( index )( index+1 )( sealedRight ) ),
            g.p_stop( StopOrNot( w.w, LeftAttachment, true ) )( Stop ) +
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
            rightArgs.foldLeft( Tuple2( null, Double.NegativeInfinity ) )( (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )( NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) )( arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            )

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
              bestArgScore + g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )
            )
        }


        // now gather each possible un-sealed leftward-looking head.
        val unsealedRightHeads = matrix( k )( end ).keySet.filter{ _.mark == UnsealedLeftFirst }
        unsealedRightHeads.foreach{ h =>
          // store the best way to get h from start to k as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.foldLeft( Tuple2( null, Double.NegativeInfinity ) )( (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj( h.obs, k ) ) )( NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, LeftAttachment ) )( arg.obs.w ) +
                matrix(start)(k)(arg).score +
                matrix(k)(end)(h).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            )

          matrix(start)(end) +=
            h -> new LeftHeadedVitSynEntry(
              Option( matrix(k)(end)(h) ),
              Option( matrix(start)(k)(bestArg) ),
              bestArgScore
            )
          // Also, store the left seal of this head
          val sealedLeft = MarkedObservation( h.obs, SealedLeft )
          matrix(start)(end) +=
            sealedRight -> new RightHeadedVitSynEntry(
              Option( matrix( start )( end )( h ) ),
              None,
              bestArgScore + g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )
            )
        }


        // gather each possible halfsealed rightward-looking head.
        val halfsealedLeftHeads = matrix( start )( k ).keySet.filter{ _.mark == SealedLeft }
        halfsealedLeftHeads.foreach{ h =>
          // store the best way to get h from start to k as the head dominating start to end.
          val Tuple2( bestArg, bestArgScore ) =
            rightArgs.foldLeft( Tuple2( null, Double.NegativeInfinity ) )( (bestArgAndScore,arg) =>
              val bestScore = bestArgAndScore._2
              val newScore =
                g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )( NotStop ) +
                g.p_choose( ChooseArgument( h.obs.w, RightAttachment ) )( arg.obs.w ) +
                matrix(start)(k)(h).score +
                matrix(k)(end)(arg).score

              if( newScore > bestScore )
                Tuple2( arg, newScore )
              else
                bestArgAndScore
            )

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
              Option( matrix( start )( end )( sealedRight ) ),
              None,
              bestArgScore + g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj( h.obs, k ) ) )
            )
        }



      }
    }
  }

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

  def constituencyParse( toParse: List[TimedSentence] ) = {
    toParse.map{ case TimedSentence( id, s ) =>
      val chart = new ViterbiChart( s )

      (1 to ( s.size )) foreach{ j =>
        chart.lexFill( j-1 )
        if( j > 1 )
          (0 to (j-2)).reverse.foreach{ i =>
            chart.synFill( i , j )
          }
      }

      id + " " + chart.toConstituencyParse
    }
  }

}
