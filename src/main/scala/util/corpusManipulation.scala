package predictabilityParsing.util

import predictabilityParsing.types.labels._
import scala.collection.mutable.{ArrayBuffer,HashMap,Map}

object CorpusManipulation {
  def allSpans( s:List[ObservedLabel] ) = {
    val allSpans = new ArrayBuffer[Yield]()

    ( 0 to s.length-1).foreach{ i =>
      (i+1 to s.length ).foreach{ j =>
        allSpans += Yield( s.slice(i,j) )
      }
    }
    allSpans.toList
  }

  def allContexts( s:List[ObservedLabel] ) = {
    val allContexts = new ArrayBuffer[Context]()

    ( 0 to s.length-1).foreach{ i =>
      (i to s.length-1 ).foreach{ j =>
        allContexts += 
          Context(
            if( i == 0 ) SentenceBoundary else s( i-1),
            if( j == s.length-1 ) SentenceBoundary else s( j+1 )
          )
      }
    }
    allContexts.toList
  }

  def spanCounts( corpus:List[List[ObservedLabel]] ) = {
    val allSpans = Map[Yield,Double]().withDefaultValue( 0D )

    corpus.foreach{ s =>
      ( 0 to s.length-1).foreach{ i =>
        (i+1 to s.length ).foreach{ j =>
          allSpans( Yield( s.slice(i,j) ) ) += 1
        }
      }
    }
    allSpans
  }

  def contextCounts( corpus:List[List[ObservedLabel]] ) = {
    val allContexts = Map[Context,Double]().withDefaultValue( 0D )

    corpus.foreach{ s =>
      ( 0 to s.length-1).foreach{ i =>
        (i to s.length-1 ).foreach{ j =>
          allContexts(
            Context(
              if( i == 0 ) SentenceBoundary else s( i-1),
              if( j == s.length-1 ) SentenceBoundary else s( j+1 )
            )
          ) += 1
        }
      }
    }
    allContexts
  }

}

