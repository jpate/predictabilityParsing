package predictabilityParsing.util
import math.{exp,log,abs}

object Math {
  // expDigamma without exp because we are in log-space
  def expDigamma( input:Double ) = {
    import math.{exp,log}
    var r = 0D
    var x = exp( input )
    while( x <= 5 ) {
      r -= 1/x
      x += 1
    }
    val f = 1/(x*x)
    val t = f*(-1D/12.0 + f*(1D/120.0 + f*(-1D/252.0 + f*(1/240.0 + f*(-1/132.0
        + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
    r + log(x) - 0.5/x + t;
  }

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
    if( abs( a - b ) < 0.000001 ) // Sigh, was still getting some underflow >_<
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


  def approxEquals( x1:Double, x2:Double, epsilon:Double = 0.00001 ) =
    math.abs( x1 - x2 ) < epsilon
}


