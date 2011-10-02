package predictabilityParsing.parsers

import predictabilityParsing.grammars.TwoContextCCMGrammar
import predictabilityParsing.partialCounts.TwoContextCCMPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.{Math,CorpusManipulation}
import math.log

class TwoContextCCMEstimator(
  val smoothTrue:Double = 2D,
  val smoothFalse:Double = 8D
) extends AbstractCCMParser[TwoContextCCM] {
  val g = new TwoContextCCMGrammar( Set[Yield](), Set[Context](), Set[Context]() )

  def setGrammar( givenGrammar:TwoContextCCMGrammar ) { g.setParams( givenGrammar ) }

  def setP_SplitGrammar( corpus:List[List[WordPair]] ) {
    // Do this in log space?
    def p_split( i:Int, j:Int, n:Int ) = {
      val l = j - i
      // if( i == j )
      //   0D
      //else
      // if( i == 0 && j == n )
      //   0D
      // else if( 0 < i && i < j && j < n )
      //   math.log( 2D ) - math.log(j - i ) + math.log( j - i + 1 )
      // else
      //   0D - math.log( j - i )
      if( i == 0 && j == n )
        1D
      else if( 0 < i && i < j && j < n )
        2D /( (j - i ) * ( j - i + 1 ) )
      else
        1D / ( j - i )
    }

    var constituentDenominator = 0D
    var distituentDenominator = 0D

    val corpusCounts = corpus.map{ s =>
      val initPartialCounts = new TwoContextCCMPartialCounts
      ( 0 to (s.length-1) ).foreach{ i =>
        ( (i+1) to (s.length) ).foreach{ j =>
          val span = Yield( s.slice( i, j ).map( _.obsA ) )

          val contextA =
            Context(
              if( i == 0 ) SentenceBoundary else s(i-1).obsA,
              if( j == (s.length) ) SentenceBoundary else s(j).obsA
            )

          val contextB =
            Context(
              if( i == 0 ) SentenceBoundary else s(i-1).obsB,
              if( j == (s.length) ) SentenceBoundary else s(j).obsB
            )


          //val thisP_split = math.log( p_split( i, j, s.length ) )
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


          initPartialCounts.incrementContextCountsA(
            Constituent,
            contextA,
            math.log( thisP_split )
          )
          initPartialCounts.incrementContextCountsA(
            Distituent,
            contextA,
            math.log( 1D - thisP_split )
          )


          initPartialCounts.incrementContextCountsB(
            Constituent,
            contextB,
            math.log( thisP_split )
          )
          initPartialCounts.incrementContextCountsB(
            Distituent,
            contextB,
            math.log( 1D - thisP_split )
          )

        }
      }

      initPartialCounts
    }.reduceLeft{ (a,b) => a.destructivePlus(b); a }

    g.setParams( corpusCounts.toTwoContextCCMGrammar( smoothTrue, smoothFalse ) )
  }

  class Entry( val span:Yield, val contextA:Context, val contextB: Context ) {
    var iScore = 0D
    var oScore = 0D
    var phiScore = g.phi( TwoContextCCM( span, contextA, contextB ) )

    def setIScore( updatedScore:Double ) { iScore = updatedScore }
    def setOScore( updatedScore:Double ) { oScore = updatedScore }

    def incrementIScore( inc:Double ) { iScore = Math.sumLogProb( iScore, inc ) }
    def incrementOScore( inc:Double ) { oScore = Math.sumLogProb( oScore, inc ) }

    override def toString = contextA.toString + "^" + contextB.toString
  }

  class LexEntry( span:Yield, contextA:Context, contextB:Context )
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
          Context(
            if( index == 0 ) SentenceBoundary else s( index-1 ).obsB,
            if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsB
          )
        )
    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = Yield( s.slice( start, end ).map( _.obsA ) )
      val thisContextA = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ).obsA,
        if( end == s.length ) SentenceBoundary else s( end ).obsA
      )
      val thisContextB = Context(
        if( start == 0 ) SentenceBoundary else s( start-1 ).obsB,
        if( end == s.length ) SentenceBoundary else s( end ).obsB
      )

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

          val thisSpan = Yield( s.slice( i, j+1 ).map( _.obsA ) )
          val thisContextA = Context(
            if( i == 0 ) SentenceBoundary else s( i-1 ).obsA,
            if( j == s.length ) SentenceBoundary else s( j ).obsA
          )
          val thisContextB = Context(
            if( i == 0 ) SentenceBoundary else s( i-1 ).obsB,
            if( j == s.length ) SentenceBoundary else s( j ).obsB
          )

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

