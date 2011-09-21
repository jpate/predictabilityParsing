package predictabilityParsing.test

import predictabilityParsing.types.labels._
import predictabilityParsing.parsers.CCMEstimator
import predictabilityParsing.util.CorpusManipulation
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class EstimatorTestSuite extends AssertionsForJUnit with Suite {

  val testCorpus = List(
    "DT VBZ NNS NNS FUZZ",
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

  val spanCounts = CorpusManipulation.spanCounts( testCorpus )
  val contextCounts = CorpusManipulation.contextCounts( testCorpus )

  @Test def testTenIterations {
    val estimator = new CCMEstimator
    // println( "Uniform" )
    // estimator.setUniformGrammar( allSpans, allContexts )
    // estimator.setUniformGrammar( spanCounts, contextCounts )
    // println( estimator.g )

    estimator.setP_SplitGrammar( testCorpus )

    // println( estimator.g )

    // println( "Initial Grammar" )
    // println( estimator.g )

    var i = 0
    var lastLogProb = 1D
    var logProbDeltas = List[Double]()
    while( i < 10 ) {

      val newPartialCounts = estimator.computePartialCounts( testCorpus )

      val corpusLogProb = newPartialCounts.getTotalScore

      val deltaLogProb = ( (lastLogProb - corpusLogProb ) / lastLogProb )

      println( "Corpus Log Prob: " + corpusLogProb + " (" + deltaLogProb + ")" )

      logProbDeltas = deltaLogProb :: logProbDeltas

      // println( "Partial counts for this iteration:" )
      // println( newPartialCounts )

      val updatedGrammar = newPartialCounts.toCCMGrammar
      // updatedGrammar.normalize
      estimator.setGrammar( updatedGrammar )
      lastLogProb = corpusLogProb
      i += 1
    }

    assertTrue( logProbDeltas.init.forall( _ > 0 ) )
  }

}


