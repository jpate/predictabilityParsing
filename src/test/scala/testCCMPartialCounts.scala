package predictabilityParsing.test

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.partialCounts.CCMPartialCounts
import predictabilityParsing.util.CorpusManipulation
import predictabilityParsing.types.labels._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import math.{abs,log}


class CCMPartialCountsTestSuite extends AssertionsForJUnit with Suite {

  val testCorpus = List(
    "DT VBZ NNS FUZZ",
    "UH FUZZ VBP DT NN NN MD VB DT JJ MD RB FUZZ",
    "IN FUZZ VBD FUZZ VBP FUZZ MD VB DT NN NN FUZZ VBP RB RB IN NNS IN DT NNS",
    "RB DT CD",
    "CC DT NN FUZZ MD RB VB UH FUZZ VBD DT NN IN DT NN JJ IN DT NN FUZZ VBP",
    "FUZZ RB VBZ",
    "CC DT JJ NN FUZZ VBP FUZZ MD VB CD CC FUZZ MD VB CD",
    "FUZZ VBZ",
    "FUZZ RB",
    "CC FUZZ VBD RB RB JJ IN FUZZ TO RB VB RP",
    "FUZZ RB VBP"
  ).map{ _.split(" ").toList.map{ Word( _ ) } }

  val allSpans = testCorpus.flatMap{ CorpusManipulation.allSpans( _ ) }.toSet
  val allContexts = testCorpus.flatMap{ CorpusManipulation.allContexts( _ ) }.toSet

  /*
  @Test def testToGrammar {
    val g = new CCMGrammar( allSpans, allContexts )
    val pc = new CCMPartialCounts

    pc.setSpansAndContexts( g.p_span, g.p_context )
    assertTrue( g == pc.toCCMGrammar )

    g.randomize( 43723 )
    assertFalse( g == pc.toCCMGrammar )
    pc.setSpansAndContexts( g.p_span, g.p_context )
    assertTrue( g == pc.toCCMGrammar )
  }
  */

  @Test def testPartialCountsAddition {
    val pc1 = new CCMPartialCounts
    val pc2 = new CCMPartialCounts

    val someSpans = Set(
      Yield( List( Word("a"), Word("b") ) ),
      Yield( List( Word("b") ) ),
      Yield( List( Word("a") ) )
    )

    val someContexts = Set(
      Context( SentenceBoundary, Word( "b" ) ),
      Context( Word("a"), SentenceBoundary ),
      Context( SentenceBoundary, SentenceBoundary )
    )

    val g = new CCMGrammar( someSpans, someContexts )

    pc1.setSpansAndContexts( g.getPSpan, g.getPContext )
    pc2.setSpansAndContexts( g.getPSpan, g.getPContext )

    val pc3 = pc1 + pc2

    val epsilon = 0.001
    assertTrue(
      (
        pc3.getSpans ==
        pc1.getSpans.union( pc2.getSpans )
      ) &&
      pc3.getSpans.forall{ span =>
        abs( pc3.getSpanCounts( Constituent , span ) - log( 2D/3D ) ) < epsilon
      } && (
        pc3.getContexts ==
        pc1.getContexts.union( pc2.getContexts )
      ) &&
      pc3.getContexts.forall{ context =>
        abs( pc3.getContextCounts( Constituent, context ) - log( 2D/3D ) ) < epsilon
      }
    )

  }

}
