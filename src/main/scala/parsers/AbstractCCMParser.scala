package predictabilityParsing.parsers

import predictabilityParsing.grammars.AbstractCCMGrammar
import predictabilityParsing.util.Math
import predictabilityParsing.types.labels._

abstract class AbstractCCMParser[T<:Parameterization] {
  //type G<:AbstractCCMParser[T]
  //type G<:AbstractCCMGrammar[T]
  val g:AbstractCCMGrammar[T]

  //def setGrammar( givenGrammar:AbstractCCMGrammar[T] ) { g.setParams( givenGrammar ) }

  /*
   * Note that, as everywhere else, this is in LOG-SPACE.
   */
  // protected def phi( span:Yield, context:Context ) =
  //   ( g.smoothedSpanScore( Constituent , span ) + g.smoothedContextScore( Constituent , context ) ) -
  //     ( g.smoothedSpanScore( Distituent , span ) + g.smoothedContextScore( Distituent , context ) )
}

