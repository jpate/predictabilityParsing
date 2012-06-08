package predictabilityParsing.test

import math.{exp,log}
import scalala.library.Numerics.logSum
import predictabilityParsing.partialCounts.DMVBayesianBackoffPartialCounts
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class DMVBayesianBackoffChart extends AssertionsForJUnit with Suite {
  val pc = new DMVBayesianBackoffPartialCounts( 1, 2 )


  val h1 = WordPair( "h", "1" )
  val h2 = WordPair( "h", "2" )
  val a1 = WordPair( "a", "1" )
  val a2 = WordPair( "a", "2" )


  pc.setStopCounts(
    StopOrNot( h1, RightAttachment, true ),
    Stop,
    math.log( 1 )
  )
  pc.setStopCounts(
    StopOrNot( h1, RightAttachment, true ),
    NotStop,
    math.log( 3 )
  )

  pc.setStopCounts(
    StopOrNot( h2, RightAttachment, true ),
    Stop,
    math.log( 10 )
  )
  pc.setStopCounts(
    StopOrNot( h2, RightAttachment, true ),
    NotStop,
    math.log( 1 )
  )




  pc.setChooseCounts(
    ChooseArgument( h1, RightAttachment ),
    a1,
    math.log( 10 )
  )

  pc.setChooseCounts(
    ChooseArgument( h1, RightAttachment ),
    a2,
    math.log( 1 )
  )


  val g = pc.toDMVGrammar


  @Test def testBothHeadKnownStop {
    println(
      "g.stopScore(" + StopOrNot( h1, RightAttachment, true ), Stop + "): " +
      log( g.stopScore( StopOrNot( h1, RightAttachment, true ), Stop ) ) + "\n\n" +
      log(
        logSum(
          {
            // Not backoff interpolation
            (
              expDigamma( log( 4 + 1 ) ) - expDigamma( log( ( 4 + 1 ) + (0 + 2) ) )
            ) + // Not backoff score
            (
              expDigamma( log( 1 + 1 ) ) - expDigamma( log( ( 1 + 1 ) + ( 3 + 1 ) ) )
            )
          },
          {
            // backoff interpolation
            (
              expDigamma( log( 0 + 2 ) ) - expDigamma( log( ( 4 + 1 ) + (0 + 2) ) )
            ) + // backoff score
            (
              expDigamma( log( 1 + 10 + 1 ) ) - expDigamma( log( ( 1 + 10 + 1 ) + ( 3 + 1 + 1 ) ) )
            )
          }
        )
      )
    )

    assertTrue(
      approxEquals(
        g.stopScore( StopOrNot( h1, RightAttachment, true ), Stop ),
        logSum(
          {
            // Not backoff interpolation
            (
              expDigamma( log( 4 + 1 ) ) - expDigamma( log( ( 4 + 1 ) + (0 + 2) ) )
            ) + // Not backoff score
            (
              expDigamma( log( 1 + 1 ) ) - expDigamma( log( ( 1 + 1 ) + ( 3 + 1 ) ) )
            )
          },
          {
            // backoff interpolation
            (
              expDigamma( log( 0 + 2 ) ) - expDigamma( log( ( 4 + 1 ) + (0 + 2) ) )
            ) + // backoff score
            (
              expDigamma( log( 1 + 10 + 1 ) ) - expDigamma( log( ( 1 + 10 + 1 ) + ( 3 + 1 + 1 ) ) )
            )
          }
        )
      )
    )
  }

}

