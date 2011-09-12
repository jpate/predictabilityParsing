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
    if( abs( a - b ) < 0.0000001 ) // Sigh, still getting some underflow >_<
      Double.NegativeInfinity
    else
      a + log( 1 - exp( b - a ) )

  def binary_bracketings_count( n:Int ) =
    catalan( n-1 )

  def catalan( n:Int ):Int =
    if( n <= 1 )
      1
    else
      catalan( n-1 ) * 2 * ((2*n)-1) / (n+1)
}


