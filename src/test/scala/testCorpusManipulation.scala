package predictabilityParsing.test

import predictabilityParsing.types.labels._
import predictabilityParsing.util.CorpusManipulation
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class CorpusManipulationTestSuite extends AssertionsForJUnit with Suite {
  val testStrings = List( "DT", "VBZ", "NNS" ).map{ Word(_) }

  @Test def testGatherSpans {
    val gatheredSpans = CorpusManipulation.allSpans( testStrings )
    val trueSpans = 
      List(
        Yield( List( Word("DT") ) ),
        Yield( List( Word("DT"), Word("VBZ") ) ),
        Yield( List( Word("DT"), Word("VBZ"), Word("NNS") ) ),
        Yield( List( Word("VBZ") ) ),
        Yield( List( Word("VBZ"), Word("NNS") ) ),
        Yield( List( Word("NNS") ) )
      )


    assertTrue( gatheredSpans == trueSpans )
  }


  @Test def testGatherContexts = {
    val gatheredContexts = CorpusManipulation.allContexts( testStrings )
    val trueContexts = 
      List(
        Context( SentenceBoundary, Word( "VBZ" ) ),
        Context( SentenceBoundary, Word( "NNS" ) ),
        Context( SentenceBoundary, SentenceBoundary ),
        Context( Word("DT"), Word( "NNS" ) ),
        Context( Word("DT"), SentenceBoundary ),
        Context( Word("VBZ"), SentenceBoundary )
      )

    assertTrue( gatheredContexts == trueContexts )
  }
}
