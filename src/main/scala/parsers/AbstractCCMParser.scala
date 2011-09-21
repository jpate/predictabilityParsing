package predictabilityParsing.parsers

import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.Math
import predictabilityParsing.types.labels._

abstract class AbstractCCMParser {
  var g:CCMGrammar

  /*
   * Note that, as everywhere else, this is in LOG-SPACE.
   */
  protected def phi( span:Yield, context:Context ) =
    ( g.smoothedSpanScore( Constituent , span ) + g.smoothedContextScore( Constituent , context ) ) -
      ( g.smoothedSpanScore( Distituent , span ) + g.smoothedContextScore( Distituent , context ) )
  // protected def phi( span:Yield, context:Context ) =
  //   ( g.p_span( Constituent )( span ) + g.p_context( Constituent )( context ) ) -
  //     ( g.p_span( Distituent )( span ) + g.p_context( Distituent )( context ) )
}

