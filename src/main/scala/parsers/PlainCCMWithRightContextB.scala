package predictabilityParsing.parsers

import predictabilityParsing.grammars.TwoContextCCMGrammar
import predictabilityParsing.partialCounts.TwoContextCCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.{Math,CorpusManipulation}
import math.log

class PlainCCMWithRightContextBEstimator(
  val smoothTrue:Double = 2D,
  val smoothFalse:Double = 8D
) extends AbstractCCMParser[TwoContextCCM] {
  val g = new TwoContextCCMGrammar( Set[Yield](), Set[Context](), Set[RightContext]() )

  def setGrammar( givenGrammar:TwoContextCCMGrammar ) { g.setParams( givenGrammar ) }

    /*
     * Sets an initial grammar where each span is equiprobable and each context is equiprobable.
     * @param spans the spans to define the grammar over.
     * @param contextsA the contextsA to define the grammar over.
     * @param contextsB the contextsB to define the grammar over.
     */
  def setUniformGrammar(
    spans:Iterable[Yield],
    contextsA:Iterable[Context],
    contextsB:Iterable[RightContext]
  ) {
    g.setParams( new TwoContextCCMGrammar( spans, contextsA, contextsB ) )
  }

    /*
     * Sets an initial grammar where the probability of each span or context is its relative
     * frequency in the given maps.
     * @param spans a map giving a count for each span.
     * @param contextsA a map giving a count for each contextA.
     * @param contextsB a map giving a count for each contextB.
     */
  def setUniformGrammar(
    spans:collection.mutable.Map[Yield,Double],
    contextsA:collection.mutable.Map[Context,Double],
    contextsB:collection.mutable.Map[RightContext,Double]
  ) {
    val pc = new TwoContextCCMPartialCounts( smoothTrue, smoothFalse )

    val totalSpans = spans.values.reduceLeft(_+_)
    val totalContextsA = contextsA.values.reduceLeft(_+_)
    val totalContextsB = contextsB.values.reduceLeft(_+_)

    spans.keySet.foreach{ span =>
      pc.incrementSpanCounts( Constituent, span, log( spans(span) / totalSpans )  )
      pc.incrementSpanCounts(
        Distituent, span, log( 1D - ( spans(span) / totalSpans ) )
      )
    }

    contextsA.keySet.foreach{ context =>
      pc.incrementContextCountsA( Constituent, context, log( contextsA(context) / totalContextsA ) )
      pc.incrementContextCountsA(
        Distituent, context, log( 1D - ( contextsA(context) / totalContextsA ) )
      )
    }

    contextsB.keySet.foreach{ context =>
      pc.incrementContextCountsB( Constituent, context, log( contextsB(context) / totalContextsB ) )
      pc.incrementContextCountsB(
        Distituent, context, log( 1D - ( contextsB(context) / totalContextsB ) )
      )
    }


    g.setParams( pc.toTwoContextCCMGrammar( smoothTrue , smoothFalse ) )
  }

  def setP_SplitGrammar( corpus:List[List[WordPair]] ) {
    // Do this in log space?
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
      //println( (0,s.length) + ": " + s )
      val initPartialCounts = new TwoContextCCMPartialCounts( smoothTrue, smoothFalse )
      ( 0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          val span = Yield( s.slice( i, j ).map{_.obsA} )

          val contextA =
            Context(
              if( i == 0 ) SentenceBoundary else s(i-1).obsA,
              if( j == (s.length) ) SentenceBoundary else s(j).obsA
            )

          val contextB = RightContext( s(j-1).obsB )

          val thisP_split = math.log( p_split( i, j, s.length ) )


          initPartialCounts.incrementSpanCounts(
            Constituent,
            span,
            thisP_split
          )
          initPartialCounts.incrementSpanCounts(
            Distituent,
            span,
            Math.subtractLogProb( 0D , thisP_split )
          )


          initPartialCounts.incrementContextCountsA(
            Constituent,
            contextA,
            thisP_split
          )
          initPartialCounts.incrementContextCountsA(
            Distituent,
            contextA,
            Math.subtractLogProb( 0D , thisP_split )
          )


          initPartialCounts.incrementContextCountsB(
            Constituent,
            contextB,
            thisP_split
          )
          initPartialCounts.incrementContextCountsB(
            Distituent,
            contextB,
            Math.subtractLogProb( 0D , thisP_split )
          )

        }
      }

      initPartialCounts
    }.reduceLeft{ (a,b) => a.destructivePlus(b); a }

    g.setParams( corpusCounts.toTwoContextCCMGrammar( smoothTrue, smoothFalse ) )
  }

  class Entry( val span:Yield, val contextA:Context, val contextB: RightContext ) {
    var iScore = 0D
    var oScore = 0D
    var phiScore = g.phi( TwoContextCCM( span, contextA, contextB ) )

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

    override def toString = contextA.toString + "^" + contextB.toString
  }

  class LexEntry( span:Yield, contextA:Context, contextB:RightContext )
    extends Entry( span, contextA, contextB ) {
    iScore = g.phi( TwoContextCCM( span, contextA, contextB ) )
  }


  /*
   * A class for charts to populate. For now, we'll only allow one constituent category, especially
   * since Klein found multiple categories hurt performance. Right now, this is just copy-pasted
   * then modified from the vanilla CCM implementation...
   */
  class Chart( s:List[WordPair] ) {
    private val matrix = Array.ofDim[Entry]( s.length+1, s.length+1 )

    def lexFill( index:Int ) {
      matrix( index )( index+1 ) =
        new LexEntry(
          Yield( ( s(index).obsA::Nil ) ),
          Context(
            if( index == 0 ) SentenceBoundary else s( index-1 ).obsA,
            if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsA
          ),
          RightContext( s(index) )
            // if( index == 0 ) SentenceBoundary else s( index-1 ).obsB,
          //   if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsB
          // )
        )
    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = Yield( s.slice( start, end ).map{ _.obsA } )
      val thisContextA = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ).obsA,
        if( end == s.length ) SentenceBoundary else s( end ).obsA
      )
      val thisContextB = RightContext( s(end-1).obsB )
        // if( start == 0 ) SentenceBoundary else s( start-1 ).obsB,
        //if( end == s.length ) SentenceBoundary else s( end ).obsB
      //)

      matrix( start )( end ) = new Entry( thisSpan, thisContextA, thisContextB )
      matrix( start )( end ).setIScore(
        g.phi( TwoContextCCM( thisSpan, thisContextA, thisContextB ) ) +
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

          val thisSpan = Yield( s.slice( i, j+1 ).map{ _.obsA } )
          val thisContextA = Context(
            if( i == 0 ) SentenceBoundary else s( i-1 ).obsA,
            if( j == s.length ) SentenceBoundary else s( j ).obsA
          )
          val thisContextB = RightContext( s( j-1 ).obsB )

          val leftSum =
            ( 0 to (i-1) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              Math.sumLogProb(
                a,
                matrix(k)(i).iScore +
                matrix(k)(j).oScore +
                g.phi(
                  TwoContextCCM(
                    matrix(k)(j).span,
                    matrix(k)(j).contextA,
                    matrix(k)(j).contextB
                  )
                )
              )
            }

          val rightSum =
            ( (j+1) to (n) ).foldLeft( Double.NegativeInfinity ){ (a, k) =>
              Math.sumLogProb(
                a,
                matrix(j)(k).iScore +
                matrix(i)(k).oScore +
                g.phi(
                  TwoContextCCM(
                    matrix(i)(k).span,
                    matrix(i)(k).contextA,
                    matrix(i)(k).contextB
                  )
                )
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
      val pc = new TwoContextCCMPartialCounts( smoothTrue, smoothFalse )

      var distituentProduct = 0D
      (0 to s.length-1).foreach{ i =>
        ( i+1 to s.length ).foreach{ j =>
          distituentProduct +=
            g.smoothedSpanScore( Distituent , matrix(i)(j).span ) +
            g.smoothedContextScoreA( Distituent , matrix(i)(j).contextA ) +
            g.smoothedContextScoreB( Distituent , matrix(i)(j).contextB )
        }
      }

      val p_tree = 0D - Math.log_space_binary_bracketings_count( s.length )
      val stringScore = matrix(0)(s.length).iScore + distituentProduct + p_tree

      val fullStringIScore = matrix(0)(s.length).iScore
      (0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to s.length ).foreach{ j =>
          val thisEntry = matrix(i)(j)
          val thisSpan = thisEntry.span
          val thisContextA = thisEntry.contextA
          val thisContextB = thisEntry.contextB
          val thisP_bracket = thisEntry.iScore + thisEntry.oScore - ( fullStringIScore )
          val thisP_noBracket = Math.subtractLogProb( 0D, thisP_bracket )

          pc.incrementSpanCounts(
            Constituent,
            thisSpan,
            thisP_bracket
          )
          pc.incrementContextCountsA(
            Constituent,
            thisContextA,
            thisP_bracket
          )
          pc.incrementContextCountsB(
            Constituent,
            thisContextB,
            thisP_bracket
          )

          pc.incrementSpanCounts(
            Distituent,
            thisSpan,
            thisP_noBracket
          )
          pc.incrementContextCountsA(
            Distituent,
            thisContextA,
            thisP_noBracket
          )
          pc.incrementContextCountsB(
            Distituent,
            thisContextB,
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
  def populateChart( s:List[WordPair] ) = {
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

  def computePartialCounts( corpus:Iterable[List[WordPair]] ) =
    corpus.par.map{ s => populateChart(s).toPartialCounts }.reduce{(a,b) => a.destructivePlus(b); a}


}

class PlainCCMWithRightContextBParser {
  val g = new TwoContextCCMGrammar( Set[Yield](), Set[Context](), Set[RightContext]() )

  def setGrammar( givenGrammar:TwoContextCCMGrammar ) { g.setParams( givenGrammar ) }

  class ViterbiEntry(
    val bestLeftChild:Option[ViterbiEntry],
    val bestRightChild:Option[ViterbiEntry],
    val score:Double
  ) {
    override def toString = "(NT " + bestLeftChild.get + " " + bestRightChild.get + " )"
  }

  class ViterbiLex( lex:WordPair, score:Double ) extends ViterbiEntry( None, None, score ) {
    override def toString = "(TERM " + lex + ")"
  }

  class ViterbiChart( s:List[WordPair] ) {
    private val matrix = Array.ofDim[ViterbiEntry]( s.length+1, s.length+1 )

    def lexFill( index:Int ) {
      matrix( index )( index+1 ) = new ViterbiLex( s(index),
        g.phi(
          TwoContextCCM(
            Yield( (s(index).obsA::Nil) ),
            Context(
              if( index == 0 ) SentenceBoundary else s( index-1 ).obsA,
              if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsA
            ),
            RightContext( s( index ).obsB )
              // if( index == 0 ) SentenceBoundary else s( index-1 ).obsB,
            //   if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsB
            // )
          )
        )
      )

    }

    val rand = new scala.util.Random();
    def synFill( start:Int, end:Int ) {
      val thisSpan = Yield( s.slice( start, end ).map{ _.obsA } )
      val thisContextA = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ).obsA,
        if( end == s.length ) SentenceBoundary else s( end ).obsA
      )
      val thisContextB = RightContext( s( end - 1 ) )
        // if( start == 0 ) SentenceBoundary else s( start-1 ).obsB,
      //   if( end == s.length ) SentenceBoundary else s( end ).obsB
      // )

      val Tuple2( bestSplit, bestSplitScore ) =
        ( (start+1) to (end-1) ).foldLeft( Tuple2(0,Double.NegativeInfinity) ){ (a,k) =>
          val thisSplitScore = matrix(start)(k).score + matrix(k)(end).score
          if( thisSplitScore > a._2 )
            Tuple2( k, thisSplitScore )
          else if( thisSplitScore < a._2 )
            a
          else
            if( rand.nextDouble > 0.5 )
              a
            else
              Tuple2( k, thisSplitScore )
        }

      matrix(start)(end) = new ViterbiEntry(
        Some( matrix(start)(bestSplit) ),
        Some( matrix(bestSplit)(end) ),
        bestSplitScore + g.phi( TwoContextCCM( thisSpan, thisContextA, thisContextB ) )
      )
    }

    override def toString = matrix(0)(s.length).toString
  }

  def parse( toParse:List[TwoStreamSentence] ) = {
    toParse.map{ case TwoStreamSentence( id, s ) =>
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
