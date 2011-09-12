package predictabilityParsing.test

import predictabilityParsing.types.labels._
import predictabilityParsing.parsers.CCMEstimator
import predictabilityParsing.util.CorpusManipulation
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ChartTestSuite extends AssertionsForJUnit with Suite {

  //val testString = "DT VBZ NNS FUZZ".split(" ").map{ Word( _ ) }.toList
  val testString = "DT VBZ NNS".split(" ").map{ Word( _ ) }.toList

  val allSpans = CorpusManipulation.allSpans( testString ).toSet
  val allContexts = CorpusManipulation.allContexts( testString ).toSet


  @Test def testToPartialCounts {
    val estimator = new CCMEstimator
    estimator.setUniformGrammar( allSpans, allContexts )

    val thisChart = estimator.populateChart( testString )


    val pc = thisChart.toPartialCounts
    val recoveredSpans = pc.getSpans
    val recoveredContexts = pc.getContexts

    assertTrue( recoveredSpans == allSpans )
    assertTrue( recoveredContexts == allContexts )

    // assertTrue(
    //   pc.getSpans.forall{ span =>
    //     pc.getSpanCounts( Constituent, span ) > Double.NegativeInfinity
    //   } &&
    //   pc.getContexts.forall{ context =>
    //     pc.getContextCounts( Constituent, context ) > Double.NegativeInfinity
    //   }
    // )

  }

}

