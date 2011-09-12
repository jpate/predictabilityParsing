package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.partialCounts.CCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.{Math,CorpusManipulation}
import math.log

class CCMEstimator extends AbstractCCMParser {

  var g = new CCMGrammar( Set[Yield](), Set[Context]() )

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


    //g = new CCMGrammar( spans, contexts )
    g = pc.toCCMGrammar
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
      if( l == 0 && l == n )
        1D
      else if( i == 0 || j == n )
        ( 1D/( j - i ) )
      else
        ( 2D/( (j - i ) * ( j - i + 1 ) ) )
    }

    var constituentDenominator = 0D
    var distituentDenominator = 0D

    val corpusCounts = corpus.map{ s =>
      val initPartialCounts = new CCMPartialCounts
      ( 0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          val span = new Yield( s.slice( i, j ) )
          val context =
            new Context(
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

      val n_s = s.length
      val totalSpanTokens = ( n_s + 1 ) * ( n_s + 2 ) / 2
      constituentDenominator += ( 2 * n_s ) - 1
      distituentDenominator += totalSpanTokens - ( ( 2 * n_s ) - 1 )

      

      initPartialCounts
    }.reduce(_+_)//.divideBy.toCCMGrammar

    corpusCounts.hallucinateCounts( 2D, 8D )

    corpusCounts.divideSpanCounts(
      Map(
        Constituent -> constituentDenominator,
        Distituent -> distituentDenominator
      )
    )

    corpusCounts.divideContextCounts(
      Map(
        Constituent -> constituentDenominator,
        Distituent -> distituentDenominator
      )
    )

    g = corpusCounts.toCCMGrammar( 0D, 0D )

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
          new Yield( s(index)::Nil ),
          new Context(
            if( index == 0 ) SentenceBoundary else s( index-1 ),
            if( index == s.length-1) SentenceBoundary else s( index + 1 )
          )
        )
    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = new Yield( s.slice( start, end ) )
      val thisContext = new Context(
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

      //if( start == 0 && end == s.length )
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

          val thisSpan = new Yield( s.slice( i, j+1 ) )
          val thisContext = new Context(
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
            //phi( thisSpan, thisContext ) + Math.sumLogProb( leftSum, rightSum )
            Math.sumLogProb( leftSum, rightSum )
          )
        }
      )
    }

    def toPartialCounts = {
      import collection.mutable.HashMap
      val pc = new CCMPartialCounts

      var distituentProduct = 0D //Double.NegativeInfinity
      (0 to s.length-1).foreach{ i =>
        ( i+1 to s.length ).foreach{ j =>
          distituentProduct +=
            g.p_span( Distituent )( matrix(i)(j).span ) +
              g.p_context( Distituent )( matrix(i)(j).context )
        }
      }

      val stringScore =
        matrix(0)(s.length).iScore +
          //math.log( Math.binary_bracketings_count( s.length ) ) +
            distituentProduct


      val rawSpanCounts = new HashMap[Yield,Int]().withDefaultValue( 0 )
      val rawContextCounts = new HashMap[Context,Int]().withDefaultValue( 0 )
      val p_bracket = Array.fill(s.length+1,s.length+1)( Double.NegativeInfinity )
      (0 to s.length-1).foreach{ i =>
        ( i+1 to s.length ).foreach{ j =>

          rawSpanCounts( matrix(i)(j).span ) += 1
          rawContextCounts( matrix(i)(j).context ) += 1

          p_bracket(i)(j) = Math.sumLogProb(
            p_bracket(i)(j),
            matrix(i)(j).iScore + matrix(i)(j).oScore - matrix(0)(s.length).iScore
          )
        }
      }

      val spanSums = new HashMap[Yield,Double]().withDefaultValue( Double.NegativeInfinity )
      val contextSums = new HashMap[Context,Double]().withDefaultValue( Double.NegativeInfinity )
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          spanSums( matrix(i)(j).span ) = Math.sumLogProb(
            spanSums( matrix(i)(j).span ),
            p_bracket( i )( j )
          )
          contextSums( matrix(i)(j).context ) = Math.sumLogProb(
            contextSums( matrix(i)(j).context ),
            p_bracket( i )( j )
          )

        }
      }

      val commonMultiplicand = math.log( 1D/( ( 2 * s.length ) - 1 ) )
      val numberOfSpans = ( s.length + 1 ) * (s.length + 2 ) / 2
      val commonDividend = math.log( numberOfSpans - (2*s.length) + 1 )


      // Adding up P_span( span | c, s )
      spanSums.keySet.foreach{ span =>
        pc.setSpanCount(
          Constituent,
          span,
          commonMultiplicand + spanSums( span )
        )

        pc.setSpanCount(
          Distituent,
          span,
          Math.subtractLogProb( math.log( rawSpanCounts( span ) ) , spanSums( span ) ) - commonDividend
        )
      }


      // Adding up P_context( context | c, s )
      contextSums.keySet.foreach{ context =>
        pc.setContextCount(
          Constituent,
          context,
          commonMultiplicand + contextSums( context )
        )

        pc.setContextCount(
          Distituent,
          context,
          Math.subtractLogProb( math.log( rawContextCounts( context ) ) , contextSums( context ) ) -
          commonDividend
        )
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

  def computePartialCounts( corpus:Iterable[List[ObservedLabel]] ) =
    corpus.par.map{ s => populateChart(s).toPartialCounts }.reduce(_+_)

}


