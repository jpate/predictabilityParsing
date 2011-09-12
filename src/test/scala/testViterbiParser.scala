package predictabilityParsing.test

import predictabilityParsing.types.labels._
import predictabilityParsing.parsers.CCMParser
import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.CorpusManipulation
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class ViterbiParserTestSuite extends AssertionsForJUnit with Suite {
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
  ).map{ _.split(" ").toList.map{ Word( _ ) } }.map{ Sentence( "sent", _ ) }

  val allSpans = testCorpus.map( _.sentence ).flatMap{ CorpusManipulation.allSpans( _ ) }.toSet
  val allContexts = testCorpus.map( _.sentence ).flatMap{ CorpusManipulation.allContexts( _ ) }.toSet

  @Test def testDoesItWork {
    val parser = new CCMParser

    val grammar = new CCMGrammar( allSpans, allContexts )

    parser.setGrammar( grammar )

    println(
      parser.parse(
        //( (1 to testCorpus.length) zip testCorpus ).map( Sentence( _._1,_._2) )
        testCorpus
      ).mkString("\t","\n\t","\n\n" )
    )
  }

}

