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
  abstract class AbstractEntry( head:TimedObservedLabel, val attStatus:AttachmentStatus ) {
    var iScore = 0D
    var oScore = 0D
    var dependent:TimedObservedLabel

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

    override def toString = context.toString
  }

  // class LexEntry( w:ObservedLabel, attStatus:AttachmentStatus ) extends AbstractEntry( w, attStatus ) {
  class LexEntry( markedObs:MarkedObservation ) extends AbstractEntry( w, attStatus ) {
    //val MarkedObservation( w, attStatus ) = markedObs
    dependent = markedObs.obs
  }

  class SynEntry(
    h:TimedObservedLabel,
    attStatus:AttachmentStatus,
    a:TimedObservedLabel
  ) extends AbstractEntry( h, attStatus ) {
    dependent = a
  }

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
        g.p_stop( w.obs )( StopOrNot( w.obs, LeftAttachment, false ) ) +
        matrix( index )( index+1 )( leftFirst ).iScore
      )



      // klein's thesis p. 108, bottom line of each (summation comes out to 0 because we have no k
      // between i and j
      val sealedRight = MarkedObservation( w, SealedRight )
      matrix( index )( index+1 ) +=
        SealedLeft -> new LexEntry( sealedRight )
      matrix( index )( index+1 )( sealedRight ).setIScore(
        g.p_stop(w.obs)( StopOrNot( w.obs, RightAttachment, false ) ) +
        matrix( index )( index+1 )( rightFirst ).iScore
      )



      val sealedBoth = MarkedObservation( w, Sealed )
      matrix( index )( index+1 ) +=
        sealedBoth -> new LexEntry( sealedBoth )
      matrix( index )( index+1 )( Sealed ).setIScore(
        Math.sumLogProb( 
          g.p_stop(w.obs)( StopOrNot( w.obs, LeftAttachment, false ) ) +
            matrix( index )( index+1 )( sealedRight ).iScore ,
          g.p_stop(w.obs)( StopOrNot( w.obs, RightAttachment, false ) ) +
            matrix( index )( index+1 )( sealedLeft ).iScore
        )
      )
    }

    def synFill( start:Int, end:Int ) {
      // no need to select attachment direction order; that occurs in span of length 1.


      // Consider left children as head of currently considered span (i.e. from start to end)
      


    }

    def outsidePass {
      import math.exp
      val n = s.length

      /*
       * Based on ccm.py from Franco M Luque's dmvccm project. Thanks!
       */
      matrix( 0 )( n ).setOScore( 0D )
      ( 1 to (n-1) ).reverse.foreach( length =>
        ( 0 to ( n - length ) ).foreach{ i =>

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

