package predictabilityParsing.parsers

import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.partialCounts.DMVPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log


class VanillaDMVEstimator {
  val g = new DMVGrammar( vocabulary = Set[Word]() )

  def setGrammar( givenGrammar:DMVGrammar ) { g.setParams( givenGrammar ) }

  //class Entry( head:ObservedLabel, val attStatus:AttachmentStatus ) {
  abstract class AbstractEntry( val head:MarkedObservation ) {
    var iScore = Double.NegativeInfinity
    var oScore = Double.NegativeInfinity
    var dependents:Iterable[Entry] = Set[Entry]()

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

    override def toString = context.toString
  }

  // class LexEntry( w:ObservedLabel, attStatus:AttachmentStatus ) extends AbstractEntry( w, attStatus ) {
  class LexEntry( markedObs:MarkedObservation ) extends AbstractEntry( markedObs )

  class SealedSynEntry( h:MarkedObservation ) extends AbstractEntry( h ) 

  abstract class SynEntry[Dir<:AttachmentDirection]( h:MarkedObservation ) extends AbstractEntry( h ) {
    def addDependency( headEntry:Entry, argEntry:Entry ) {
      assert( headEntry.head == h )
      dependents += argEntry
      incrementIScore(
        g.p_stop( StopOrNot( h.obs, Dir, math.abs( h.obs.t - argEntry.head.obs.t) == 1 ) )( Stop ) +
        g.p_choose( ChooseArgument( h.obs, Dir ) )( argEntry.head.obs ) +
        argEntry.iScore +
        headEntry.iScore
      )
    }
  }

  class LeftHeadedSynEntry( h:MarkedObservation ) extends SynEntry[RightAttachment]( h ) 

  class RightHeadedSynEntry( h:MarkedObservation ) extends SynEntry[LeftAttachment]( h )

  /*
   * A class for charts to populate. For now, we'll only allow one constituent category, especially
   * since Klein found multiple categories hurt performance.
   */
  class Chart( s:List[TimedObservedLabel] ) {
    private val matrix = Array.fill( s.length+1, s.length+1 )( mutable.Map[MarkedObservation,Entry]() )

    def lexFill( index:Int ) {

      private val w = s(index)

      val leftFirst = MarkedObservation( w, UnsealedLeftFirst )
      matrix( index )( index+1 ) +=
        leftFirst -> new LexEntry( leftFirst )
      matrix( index )( index+1 )( leftFirst ).setIScore( g.p_order(w.obs)(UnsealedLeftFirst) )


      val rightFirst = MarkedObservation( w, UnsealedRightFirst )
      matrix( index )( index+1 ) +=
        rightFirst -> new LexEntry( rightFirst )
      matrix( index )( index+1 )( rightFirst ).setIScore( g.p_order(w.obs)(UnsealedRightFirst) )


      // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
      // between i and j
      val sealedLeft = MarkedObservation( w, SealedLeft )
      matrix( index )( index+1 ) +=
        sealedLeft -> new LexEntry( sealedLeft )
      matrix( index )( index+1 )( sealedLeft ).setIScore(
        g.p_stop( StopOrNot( w.obs, LeftAttachment, true ) )( Stop ) +
        matrix( index )( index+1 )( leftFirst ).iScore
      )



      // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
      // between i and j
      val sealedRight = MarkedObservation( w, SealedRight )
      matrix( index )( index+1 ) +=
        sealedRight -> new LexEntry( sealedRight )
      matrix( index )( index+1 )( sealedRight ).setIScore(
        g.p_stop( StopOrNot( w.obs, RightAttachment, true ) )( Stop ) +
        matrix( index )( index+1 )( rightFirst ).iScore
      )



      val sealedBoth = MarkedObservation( w, Sealed )
      matrix( index )( index+1 ) +=
        sealedBoth -> new LexEntry( sealedBoth )
      matrix( index )( index+1 )( Sealed ).setIScore(
        Math.sumLogProb( 
          g.p_stop( StopOrNot( w.obs, LeftAttachment, true ) )( Stop ) +
            matrix( index )( index+1 )( sealedRight ).iScore ,
          g.p_stop( StopOrNot( w.obs, RightAttachment, true ) )( Stop ) +
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
            matrix( start )( end ) += h -> new LeftHeadedSynEntry( h )

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
            matrix( start )( end ) += h -> new RightHeadedSynEntry( h )

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
            matrix( start )( end ) += h -> new LeftHeadedSynEntry( h )

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
            matrix( start )( end ) += h -> new RightHeadedSynEntry( h )

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
          g.p_stop( StopOrNot( h.obs, LeftAttachment, math.abs( start - h.obs.t ) <= 1 ) )( Stop ),
          matrix( start )( end )( MarkedObservation( h.obs, UnsealedLeftFirst ) ).iScore
        )
      }

      // now increment by probability of sealing an unsealed right-first head to the to the right
      val fullSpanHalfSealedRightHeads = matrix( start )( end ).keySet.filter{ _.mark == SealedRight }
      fullSpanHalfSealedRightHeads.foreach{ h =>
        matrix( start )( end )( h ).incrementIScore(
          g.p_stop( StopOrNot( h.obs, RightAttachment, math.abs( end - h.obs.t ) <= 1 ) )( Stop ),
          matrix( start )( end )( MarkedObservation( h.obs, UnsealedRightFirst ) ).iScore
        )
      }



      assert( fullSpanHalfSealedLeftHeads.map{_.obs} == fullSpanHalfSealedRightHeads.map{_.obs} )
      // Finally, we do sealed heads for current span (very top of p. 107)
      fullSpanHalfSealedLeftHeads.map{_.obs}.foreach{ hObs =>
        val h = MarkedObservation( hObs, Sealed )
        val sealedLeft = MarkedObservation( hObs, SealedLeft )
        val sealedRight = MarkedObservation( hObs, SealedRight )
        matrix( start )( end ) += h -> new SealedSynEntry( h )
        matrix( start )( end )( h ).dependents ++=
          Set( matrix( start )( end )( sealedLeft ) , matrix( start )( end)( sealedRight ) )

        matrix( start )( end )( h ).incrementIScore(
          Math.sumLogProb(
            g.p_stop( StopOrNot( hObs, LeftAttachment, math.abs( start - hobs.t ) <= 1 ) )( Stop ) +
              matrix( start )( end )( sealedRight ).iScore,
            g.p_stop( StopOrNot( hObs, RightAttachment, math.abs( end - hobs.t ) <= 1 ) )( Stop ) +
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

      matrix( 0 )( n )( MarkedObservation( Root(n-1), Sealed ) ).setOScore( 0D )
      // 1 to (n) rather than 1 to (n-1) because we do have unary branches over the whole sentence
      ( 1 to n ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>
          val j = i + length

          // First, consider the possibility that sealed nodes of current span are arguments of
          // heads outside of the span, but only if we aren't considering the full-sentence span.
          // Probably this check can be eliminated, since (0 to -1) and (n+1 to n) are empty

          if( length < n ) {

            val curSpanSealedNodes = matrix(i)(j).filter{ _.mark == Sealed }

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
                    g.p_stop( StopOrNot( h.obs.w, RightAttachment, adj(h.obs.t,i) ) )( NotStop ) +
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
                    g.p_stop( StopOrNot( h.obs.w, LeftAttachment, adj(h.obs.t,j) ) )( NotStop ) +
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
                    g.p_choose( ChooseArgument( w, LeftAttachment ) )( a.obs.w ) +
                    matrix(k)(i)(a).iScore +
                    matrix(k)(j)(w).oScore
                  )
                }
              }
            }

            // TODO START FROM HERE!
            // Unsealed leftward looking first. TODO: collapse this into above by introducing a
            // sealed() method that produces e.g. SealedRight from UnsealedRightFirst ??
            val curSpanUnsealedRightFirst = matrix( i )( j ).keySet.filter{ _.mark == UnsealedRightFirst }
            curSpanUnsealedRightFirst.foreach{ w =>
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
                    g.p_choose( ChooseArgument( w, LeftAttachment ) )( a.obs.w ) +
                    matrix(k)(i)(a).iScore +
                    matrix(k)(j)(w).oScore
                  )
                }
              }
            }

          }
        }
      )
    }

    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = new DMVPartialCounts( smoothTrue, smoothFalse )


      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>

          // increment everything from the partial counts

        }
      }

      pc.setTotalScore( treeScore )

      pc
    }

    override def toString =
      matrix.map{ row =>
        row.map{ x => if( x == null ) "    " else x.span }.mkString("<",">\t\t<",">\n") +
        row.map{ x => if( x == null ) "    " else x.context }.mkString("<",">\t\t<",">\n") +
        row.map{ x => if( x == null ) "    " else x.iScore }.mkString("<",">\t\t<",">\n") +
        row.map{ x => if( x == null ) "    " else x.oScore }.mkString("<",">\t\t<",">\n\n")
      }.mkString("\n","\n","\n\n")
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

