package predictabilityParsing.util

object Math {
  def sumLogProb( a:Double, b:Double ) =
    if( a == Double.NegativeInfinity )
      b
    else if( b == Double.NegativeInfinity )
      a
    else if( b < a )
      a + math.log( 1 + math.exp( b - a ) )
    else
      b + math.log( 1 + math.exp( a - b ) )

  def binary_bracketings_count( n:Int ) =
    catalan( n-1 )

  def catalan( n:Int ):Int =
    if( n <= 1 )
      1
    else
      catalan( n-1 ) * 2 * ((2*n)-1) / (n+1)
}


