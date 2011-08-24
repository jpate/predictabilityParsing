package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.Math
import predictabilityParsing.types.labels._

abstract class AbstractCCMParser {
  var g:CCMGrammar

  /*
   * Note that, as everywhere else, this is in LOG-SPACE.
   * TODO: have an abstract parser class so I don't just copy-paste phi
   */
  protected def phi( span:Yield, context:Context ) =
    ( g.p_span( Constituent )( span ) + g.p_context( Constituent )( context ) ) -
      ( g.p_span( Distituent )( span ) + g.p_context( Distituent )( context ) )
}

