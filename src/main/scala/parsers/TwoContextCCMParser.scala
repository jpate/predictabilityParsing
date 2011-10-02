package predictabilityParsing.parsers

import predictabilityParsing.grammars.TwoContextCCMGrammar
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log

class TwoContextCCMParser {
  val g = new TwoContextCCMGrammar( Set[Yield](), Set[Context](), Set[Context]() )

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
            Context(
              if( index == 0 ) SentenceBoundary else s( index-1 ).obsB,
              if( index == s.length-1) SentenceBoundary else s( index + 1 ).obsB
            )
          )
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
