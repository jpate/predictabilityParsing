package predictabilityParsing.test

import predictabilityParsing.types.tables._
import predictabilityParsing.types.labels._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class TableTestSuite extends AssertionsForJUnit with Suite {
  val testCPT = new LogCPT(
    Set( Word( "a" ), Word( "b" ) ),
    Set( Word( "x" ), Word( "y" ), Word( "z" ) )
  )

  val testPT = new LogPT(
    Set( Word( "a" ), Word( "b" ), Word( "x" ), Word( "y" ), Word( "z" ) )
  )

  // Maintaining a sort was getting expensive and irritating, and I don't need it for this model.
  // @Test def testPTToArray {
  //   assertTrue(
  //     testPT.toLogArray.map{ math.exp(_) }.zip( testPT.toArray ).forall{ case (a,b) => a == b }
  //   )

  //   testCPT.randomize( 8291 )

  //   assertTrue(
  //     testPT.toLogArray.map{ math.exp(_) }.zip( testPT.toArray ).forall{ case (a,b) => a == b }
  //   )
  // }

  // @Test def testCPTToArray {
  //   assertTrue(
  //     testCPT.toLogArray.map{ math.exp(_) }.zip( testCPT.toArray ).forall{case (a,b) => a == b }
  //   )

  //   testCPT.randomize( 13451 )

  //   assertTrue(
  //     testCPT.toLogArray.map{ math.exp(_) }.zip( testCPT.toArray ).forall{case (a,b) => a == b }
  //   )
  // }
}


