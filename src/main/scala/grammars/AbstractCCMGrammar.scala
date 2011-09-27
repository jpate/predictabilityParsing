package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

abstract class AbstractCCMGrammar[+T<:Parameterization](
  hallucinatedTrue:Double = 2D,
  hallucinatedFalse:Double = 8D
) {
  def phi[U>:T]( elements:U ):Double
  def randomize( seed:Int, centeredOn:Int ):Unit
  def normalize:Unit
  // def setParams[G>:T]( otherGram:AbstractCCMGrammar[G] ):Unit

  protected def defaultTrue = hallucinatedTrue / ( hallucinatedTrue + hallucinatedFalse )
  protected def defaultFalse = hallucinatedFalse / ( hallucinatedTrue + hallucinatedFalse )

}

