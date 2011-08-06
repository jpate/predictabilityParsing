package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.partialCounts.CCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math

class CCMEstimator {

  var g = new CCMGrammar( Set[Yield](), Set[Context]() )

  def setUniformGrammar( spans:Iterable[Yield], contexts:Iterable[Context] ) {
    g = new CCMGrammar( spans, contexts )
  }

  def setRandomGrammar(
    spans:Iterable[Yield],
    contexts:Iterable[Context],
    seed:Int,
    centeredOn:Int
  ) {
    g = new CCMGrammar( spans, contexts )
    g.randomize( seed, centeredOn )
  }

  def setRandomGrammar(
    spans:Iterable[Yield],
    contexts:Iterable[Context],
    seed:Int
  ) {
    g = new CCMGrammar( spans, contexts )
    g.randomize( seed )
  }

  def setGrammar( givenGrammar:CCMGrammar ) { g = givenGrammar }


  class Entry( val span:Yield, val context:Context ) {
    var iScore = Double.NegativeInfinity
    var oScore = Double.NegativeInfinity

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

    override def toString = context.toString
  }

  class LexEntry( span:Yield, context:Context ) extends Entry( span, context  ) {
    iScore = phi( span, context )
  }

  /*
   * Note that, as everywhere else, this is in LOG-SPACE.
   */
  private def phi( span:Yield, context:Context ) =
    ( g.p_span( Constituent )( span ) + g.p_context( Constituent )( context ) ) -
      ( g.p_span( Distituent )( span ) + g.p_context( Distituent )( context ) )

  /*
   * A class for charts to populate. For now, we'll only allow one constituent category, especially
   * since Klein found multiple categories hurt performance.
   */
  class Chart( s:List[ObservedLabel] ) {
    private val matrix = Array.ofDim[Entry]( s.length+1, s.length+1 )

    def lexFill( index:Int ) {
      matrix( index )( index+1 ) =
        new LexEntry(
          Yield( s(index)::Nil ),
          Context(
            if( index == 0 ) SentenceBoundary else s( index-1 ),
            if( index == s.length-1) SentenceBoundary else s( index + 1 )
          )
        )
    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = Yield( s.slice( start, end ) )
      val thisContext = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ),
        if( end == s.length ) SentenceBoundary else s( end )
      )

      matrix( start )( end ) = 
        new Entry(
          thisSpan,
          thisContext
        )

      matrix( start )( end ).setIScore(
        phi( thisSpan, thisContext ) +
        ( (start+1) to (end - 1 ) ).map{ k =>
          matrix( start )( k ).iScore + matrix( k )( end ).iScore
        }.reduceLeft{ Math.sumLogProb( _, _ ) }
      )

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
          val j = i + length

          val thisSpan = Yield( s.slice( i, j ) )
          val thisContext = Context(
            if( i == 0 ) SentenceBoundary else s( i-1 ),
            if( j == s.length ) SentenceBoundary else s( j )
          )

          val leftSum =
              ( 0 to (i-1) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
                Math.sumLogProb( a, matrix(k)(i).iScore + matrix(k)(j).oScore )
              }

          val rightSum =
              ( (j+1) to (n) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
                Math.sumLogProb( a, matrix(j)(k).iScore + matrix(i)(k).oScore )
              }

          matrix(i)(j).setOScore(
            phi( thisSpan, thisContext ) + Math.sumLogProb( leftSum, rightSum )
          )
        }
      )

    }

    def toPartialCounts = {
      val pc = new CCMPartialCounts

      val stringScore = matrix(0)(s.length).iScore

      matrix.flatMap( x => x ).filter( _ != null).foreach{ entry =>
        val entryConstituencyScore = entry.iScore + entry.oScore - stringScore
        pc.incrementSpanCounts( Constituent, entry.span, entryConstituencyScore )
        pc.incrementSpanCounts( Distituent, entry.span, 1 - entryConstituencyScore )
        pc.incrementContextCounts( Constituent, entry.context, entryConstituencyScore )
        pc.incrementContextCounts( Distituent, entry.context, 1 - entryConstituencyScore )
      }

      pc.totalScore = stringScore
      pc
    }

    override def toString =
      matrix.map{
        _.map{ x => if( x == null ) "    " else x }.mkString("<",">\t\t<",">")
      }.mkString("\n","\n","\n\n")
  }

  /*
  * This is the CYK parsing algorithm.
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with labels and inside and outside probabilities.
  */
  def populateChart( s:List[ObservedLabel] ) = {
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

  def computePartialCountsSingle( s:List[ObservedLabel] ) = populateChart( s ).toPartialCounts

  def computePartialCounts( corpus:List[List[ObservedLabel]] ) =
    corpus.par.map{ s =>
      val pc = populateChart(s).toPartialCounts
      pc
    }.reduceLeft(_+_)

}


