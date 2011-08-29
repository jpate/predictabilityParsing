package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
import math.log

class CCMParser extends AbstractCCMParser {
  var g = new CCMGrammar( Set[Yield](), Set[Context]() )

  def setGrammar( givenGrammar:CCMGrammar ) { g = givenGrammar }

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
      matrix( index )( index+1 ) = new ViterbiLex(
        s(index),
        phi(
          new Yield( s(index)::Nil ),
          new Context(
            if( index == 0 ) SentenceBoundary else s( index-1 ),
            if( index == s.length-1) SentenceBoundary else s( index + 1 )
          )
        )
      )

    }

    def synFill( start:Int, end:Int ) {
      val thisSpan = new Yield( s.slice( start, end ) )
      val thisContext = new Context(
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
        bestSplitScore + phi( thisSpan, thisContext )
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

