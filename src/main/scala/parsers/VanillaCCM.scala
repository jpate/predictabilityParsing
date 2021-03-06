package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.partialCounts.CCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.CorpusManipulation
import predictabilityParsing.util.Math._
import math.log

class VanillaCCMEstimator(
  smoothTrue:Double = 2D,
  smoothFalse:Double = 8D
) extends AbstractCCMParser[BaseCCM] {

  val g = new CCMGrammar(
    spans = Set[Yield](),
    contexts = Set[Context](),
    hallucinatedTrue = smoothTrue,
    hallucinatedFalse = smoothFalse
  )

    /*
     * Sets an initial grammar where each span is equiprobable and each context is equiprobable.
     * @param spans the spans to define the grammar over.
     * @param contexts the contexts to define the grammar over.
     */
  def setUniformGrammar( spans:Iterable[Yield], contexts:Iterable[Context] ) {
    g.setParams( new CCMGrammar( spans, contexts ) )
  }

    /*
     * Sets an initial grammar where the probability of each span or context is it's relative
     * frequency in the given maps.
     * @param spans a map giving a count for each span.
     * @param contexts a map giving a count for each context.
     */
  def setUniformGrammar( spans:collection.mutable.Map[Yield,Double], contexts:collection.mutable.Map[Context,Double] ) {
    val pc = new CCMPartialCounts( smoothTrue, smoothFalse )

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


    g.setParams( pc.toCCMGrammar( smoothTrue, smoothFalse ) )
  }

    /*
     * Sets an initial grammar where probabilities are determined randomly.
     * @param spans possible spans
     * @param contexts possible contexts
     * @param seed seed for the psueudorandom number generator.
     * @param centeredOn This is added to each random number (which are in [0,1]). Should be
     * large-ish for an approximately uniform grammar.
     */
  def setRandomGrammar(
    spans:Iterable[Yield],
    contexts:Iterable[Context],
    seed:Int,
    centeredOn:Int
  ) {
    g.setParams( new CCMGrammar( spans, contexts, smoothTrue, smoothFalse ) )
    g.randomize( seed, centeredOn )
  }

  /*
   * Sets an initial grammar where probabilities are determined randomly.
   * @param spans possible spans
   * @param contexts possible contexts
   * @param seed seed for the psueudorandom number generator.
   */
  def setRandomGrammar(
    spans:Iterable[Yield],
    contexts:Iterable[Context],
    seed:Int
  ) {
    g.setParams( new CCMGrammar( spans, contexts, smoothTrue, smoothFalse) )
    g.randomize( seed )
  }

  def setGrammar( givenGrammar:CCMGrammar ) { g.setParams( givenGrammar ) }

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
      val initPartialCounts = new CCMPartialCounts( smoothTrue, smoothFalse )
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

    g.setParams( corpusCounts.toCCMGrammar( smoothTrue, smoothFalse ) )
  }

  class Entry( val span:Yield, val context:Context ) {
    var iScore = 0D
    var oScore = 0D
    var phiScore = g.phi( BaseCCM( span, context ) )

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = logSum( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = logSum( oScore, inc ) }

    override def toString = context.toString
  }

  class LexEntry( span:Yield, context:Context ) extends Entry( span, context  ) {
    iScore = g.phi( BaseCCM( span, context ) )
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
        g.phi( BaseCCM( thisSpan, thisContext ) ) +
        ( (start+1) to (end - 1 ) ).map{ k =>
          matrix( start )( k ).iScore + matrix( k )( end ).iScore
        }.reduceLeft{ logSum( _, _ ) }
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

          val leftSum =
            ( 0 to (i-1) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              logSum(
                a,
                matrix(k)(i).iScore +
                matrix(k)(j).oScore +
                g.phi( BaseCCM( matrix(k)(j).span, matrix(k)(j).context ) )
              )
            }

          val rightSum =
            ( (j+1) to (n) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              logSum(
                a,
                matrix(j)(k).iScore +
                matrix(i)(k).oScore +
                g.phi( BaseCCM( matrix(i)(k).span, matrix(i)(k).context ) )
              )
            }

          matrix(i)(j).setOScore( logSum( leftSum, rightSum ) )
        }
      )
    }

    def toPartialCounts = {
      val pc = new CCMPartialCounts( smoothTrue, smoothFalse )

      var distituentProduct = 0D
      (0 to s.length-1).foreach{ i =>
        ( i+1 to s.length ).foreach{ j =>
          distituentProduct +=
            g.smoothedSpanScore( Distituent , matrix(i)(j).span ) +
            g.smoothedContextScore( Distituent , matrix(i)(j).context )
        }
      }

      val p_tree = 0D - log_space_binary_bracketings_count( s.length )
      val fullStringIScore = matrix(0)(s.length).iScore
      val treeScore = fullStringIScore + distituentProduct + p_tree

      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          val thisEntry = matrix(i)(j)
          val thisSpan = thisEntry.span
          val thisContext = thisEntry.context
          val thisP_bracket = thisEntry.iScore + thisEntry.oScore - ( fullStringIScore )
          val thisP_noBracket = subtractLogProb( 0D, thisP_bracket )

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

class VanillaCCMParser {
  val g = new CCMGrammar( Set[Yield](), Set[Context]() )

  def setGrammar( givenGrammar:CCMGrammar ) { g.setParams( givenGrammar ) }

  class ViterbiEntry(
    val bestLeftChild:Option[ViterbiEntry],
    val bestRightChild:Option[ViterbiEntry],
    val score:Double
  ) {
    override def toString = "(NT " + bestLeftChild.get + " " + bestRightChild.get + " )"
  }

  class ViterbiLex( lex:ObservedLabel, score:Double ) extends ViterbiEntry( None, None, score ) {
    override def toString = "(TERM " + lex + ")"
  }

  class ViterbiChart( s:List[ObservedLabel] ) {
    private val matrix = Array.ofDim[ViterbiEntry]( s.length+1, s.length+1 )

    def lexFill( index:Int ) {
      matrix( index )( index+1 ) = new ViterbiLex( s(index),
        g.phi(
          BaseCCM(
            Yield( s(index)::Nil ),
            Context(
              if( index == 0 ) SentenceBoundary else s( index-1 ),
              if( index == s.length-1) SentenceBoundary else s( index + 1 )
            )
          )
        )
      )

    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = Yield( s.slice( start, end ) )
      val thisContext = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ),
        if( end == s.length ) SentenceBoundary else s( end )
      )

      val Tuple2( bestSplit, bestSplitScore ) =
        ( (start+1) to (end-1) ).foldLeft( Tuple2(0,Double.NegativeInfinity) ){ (a,k) =>
          val thisSplitScore = matrix(start)(k).score + matrix(k)(end).score
          if( thisSplitScore > a._2 )
            Tuple2( k, thisSplitScore )
          else
            a
        }

      matrix(start)(end) = new ViterbiEntry(
        Some( matrix(start)(bestSplit) ),
        Some( matrix(bestSplit)(end) ),
        bestSplitScore + g.phi( BaseCCM( thisSpan, thisContext ) )
      )
    }

    override def toString = matrix(0)(s.length).toString
  }

  def parse( toParse:List[Sentence] ) = {
    toParse.map{ case Sentence( id, s ) =>
      val chart = new ViterbiChart( s )

      (1 to ( s.size )) foreach{ j =>
        chart.lexFill( j-1 )
        if( j > 1 )
          (0 to (j-2)).reverse.foreach{ i =>
            chart.synFill( i , j )
          }
      }

      id + " " + chart.toString
    }
  }

}

