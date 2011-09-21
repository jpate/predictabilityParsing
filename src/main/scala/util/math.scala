package predictabilityParsing.util
import math.{exp,log,abs}

object Math {
  def sumLogProb( a:Double, b:Double ) =
    if( a == Double.NegativeInfinity )
      b
    else if( b == Double.NegativeInfinity )
      a
    else if( b < a )
      a + log( 1 + exp( b - a ) )
    else
      b + log( 1 + exp( a - b ) )

  // subtraction isn't commutative so I think I just need to do this straight
  def subtractLogProb( a:Double, b:Double ) =
    if( abs( a - b ) < 0.000000001 ) // Sigh, was still getting some underflow >_<
      Double.NegativeInfinity
    else
      a + log( 1 - exp( b - a ) )

  def log_space_binary_bracketings_count( n:Int ) =
    log_space_catalan( n - 1 )

  def log_space_catalan( n:Int ):Double =
    if( n <= 1 )
      0D
    else
      log_space_catalan( n - 1 ) +
      math.log( 2 ) +
      math.log( 2 * n - 1 ) -
      math.log( n + 1 )


  def binary_bracketings_count( n:Int ) =
    catalan( n-1 )

  def catalan( n:Int ):Int =
    if( n <= 1 )
      1
    else
      catalan( n-1 ) * 2 * ((2*n)-1) / (n+1)
}


