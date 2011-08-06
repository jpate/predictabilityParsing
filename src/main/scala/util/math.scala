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
}


