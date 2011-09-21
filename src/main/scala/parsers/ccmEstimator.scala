package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.partialCounts.CCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.{Math,CorpusManipulation}
import math.log

class CCMEstimator( val smoothTrue:Double = 2D, val smoothFalse:Double = 8D ) extends AbstractCCMParser {

  var g = new CCMGrammar( spans = Set[Yield](), contexts = Set[Context]() )

  def setUniformGrammar( spans:Iterable[Yield], contexts:Iterable[Context] ) {
    g = new CCMGrammar( spans, contexts )
  }

  def setUniformGrammar( spans:collection.mutable.Map[Yield,Double], contexts:collection.mutable.Map[Context,Double] ) {
    val pc = new CCMPartialCounts

    val totalSpans = spans.values.reduceLeft(_+_)
    val totalContexts = contexts.values.reduceLeft(_+_)

    spans.keySet.foreach{ span =>
      pc.incrementSpanCounts( Constituent, span, log( spans(span) / totalSpans )  )
      pc.incrementSpanCounts(
        Distituent, span, log( 1D - ( spans(span) / totalSpans ) )
      )
    }

    contexts.keySet.foreach{ context =>
      pc.incrementContextCounts( Constituent, context, log( contexts(context) / totalContexts ) )
      pc.incrementContextCounts(
        Distituent, context, log( 1D - ( contexts(context) / totalContexts ) )
      )
    }


    g = pc.toCCMGrammar( math.log( smoothTrue ), math.log( smoothFalse ) ) 
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

  def setP_SplitGrammar( corpus:List[List[ObservedLabel]] ) {
    def p_split( i:Int, j:Int, n:Int ) = {
      val l = j - i
      if( i == j )
        0D
      else if( i == 0 && j == n )
        1D
      else if( 0 < i && i < j && j < n )
        ( 2D/( (j - i ) * ( j - i + 1 ) ) )
      else
        1D/( j - i )
    }

    var constituentDenominator = 0D
    var distituentDenominator = 0D

    val corpusCounts = corpus.map{ s =>
      val initPartialCounts = new CCMPartialCounts
      ( 0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          val span = Yield( s.slice( i, j ) )
          val context =
            Context(
              if( i == 0 ) SentenceBoundary else s(i-1),
              if( j == (s.length) ) SentenceBoundary else s(j)
            )

          val thisP_split = p_split( i, j, s.length )
          initPartialCounts.incrementSpanCounts(
            Constituent,
            span,
            math.log( thisP_split )
          )

          initPartialCounts.incrementSpanCounts(
            Distituent,
            span,
            math.log( 1D - thisP_split )
          )


          initPartialCounts.incrementContextCounts(
            Constituent,
            context,
            math.log( thisP_split )
          )
          initPartialCounts.incrementContextCounts(
            Distituent,
            context,
            math.log( 1D - thisP_split )
          )
        }
      }

      initPartialCounts
    }.reduceLeft{ (a,b) => a.destructivePlus(b); a }

    g = corpusCounts.toCCMGrammar( math.log( smoothTrue ), math.log( smoothFalse ) )
  }

  class Entry( val span:Yield, val context:Context ) {
    var iScore = 0D
    var oScore = 0D
    var phiScore = phi( span, context )

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

      matrix( start )( end ) = new Entry( thisSpan, thisContext )

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

          val thisSpan = Yield( s.slice( i, j+1 ) )
          val thisContext = Context(
            if( i == 0 ) SentenceBoundary else s( i-1 ),
            if( j == s.length ) SentenceBoundary else s( j )
          )

          val leftSum =
            ( 0 to (i-1) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              Math.sumLogProb(
                a,
                matrix(k)(i).iScore +
                matrix(k)(j).oScore +
                phi( matrix(k)(j).span, matrix(k)(j).context )
              )
            }

          val rightSum =
            ( (j+1) to (n) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              Math.sumLogProb(
                a,
                matrix(j)(k).iScore +
                matrix(i)(k).oScore +
                phi( matrix(i)(k).span, matrix(i)(k).context )
              )
            }

          matrix(i)(j).setOScore(
            Math.sumLogProb( leftSum, rightSum )
          )
        }
      )
    }

    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = new CCMPartialCounts

      var distituentProduct = 0D
      (0 to s.length-1).foreach{ i =>
        ( i+1 to s.length ).foreach{ j =>
          distituentProduct +=
            g.smoothedSpanScore( Distituent , matrix(i)(j).span ) +
            g.smoothedContextScore( Distituent , matrix(i)(j).context )
        }
      }

      val p_tree = 0D - Math.log_space_binary_bracketings_count( s.length )
      val stringScore = matrix(0)(s.length).iScore + distituentProduct + p_tree

      val fullStringIScore = matrix(0)(s.length).iScore
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          val thisEntry = matrix(i)(j)
          val thisSpan = thisEntry.span
          val thisContext = thisEntry.context
          val thisP_bracket = thisEntry.iScore + thisEntry.oScore - ( fullStringIScore )
          val thisP_noBracket = Math.subtractLogProb( 0D, thisP_bracket )

          pc.incrementSpanCounts(
            Constituent,
            thisSpan,
            thisP_bracket
          )
          pc.incrementContextCounts(
            Constituent,
            thisContext,
            thisP_bracket
          )

          pc.incrementSpanCounts(
            Distituent,
            thisSpan,
            thisP_noBracket
          )
          pc.incrementContextCounts(
            Distituent,
            thisContext,
            thisP_noBracket
          )

        }
      }

      pc.setTotalScore( stringScore )

      pc
    }

    override def toString =
      matrix.map{ row =>
        row.map{ x => if( x == null ) "    " else x }.mkString("<",">\t\t<",">\n") +
        row.map{ x => if( x == null ) "    " else x.iScore }.mkString("<",">\t\t<",">\n") +
        row.map{ x => if( x == null ) "    " else x.oScore }.mkString("<",">\t\t<",">\n\n")
      }.mkString("\n","\n","\n\n")
  }

  /*
  * This is the CYK parsing algorithm.
  * @param s The input sentence (an array of terminals)
  * @return A parse chart with inside and outside probabilities.
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

  def computePartialCounts( corpus:Iterable[List[ObservedLabel]] ) =
    corpus.par.map{ s => populateChart(s).toPartialCounts }.reduce{(a,b) => a.destructivePlus(b); a}

}


