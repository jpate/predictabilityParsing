package predictabilityParsing.test

import predictabilityParsing.types.tables._
import predictabilityParsing.types.labels._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import math.log

class TableTestSuite extends AssertionsForJUnit with Suite {
  val testCPT = new LogCPT(
    Set( Word( "a" ), Word( "b" ) ),
    Set( Word( "x" ), Word( "y" ), Word( "z" ) )
  )

  val testPT = new LogPT(
    Set( Word( "a" ), Word( "b" ), Word( "x" ), Word( "y" ), Word( "z" ) )
  )







  @Test def testDefaultValues {

    val car = Word( "car" )
    val bike = Word( "bike" )
    val shoes = Word( "shoes" )

    assertTrue( testCPT( car, bike ) == Double.NegativeInfinity )

    testCPT.setDefault( log( 0.1 ) )

    val epsilon = 0.00000
    assertTrue( testCPT( car, bike ) - log(0.1 ) <= epsilon )
    assertTrue( testCPT( shoes, bike ) - log(0.1 ) <= epsilon )

    testCPT.setDefaultMap( collection.immutable.Map( car -> log( 0.3 ), shoes -> log( 0.5 ) ) )

    // see if new default map works
    assertTrue( testCPT( car, bike ) - log(0.3 ) <= epsilon )
    assertTrue( testCPT( shoes, bike ) - log(0.5 ) <= epsilon )


    // try again with a mutable.Map
    testCPT.setDefaultMap( collection.mutable.Map( car -> log( 0.8 ), shoes -> log( 0.6 ) ) )

    // see if new default map works
    assertTrue( testCPT( car, bike ) - log(0.8) <= epsilon )
    assertTrue( testCPT( shoes, bike ) - log(0.6) <= epsilon )

    // the defaultVal should still apply for parents we've never seen.
    assertTrue( testCPT( bike, shoes ) - log(0.1) <= epsilon )

    testCPT.setValue( bike, shoes, log( 0.45 ) )

    // Also, we should be able to access values that have actually been stored...
    assertTrue( testCPT( bike, shoes ) - log( 0.45 ) <= epsilon )

  }




}


