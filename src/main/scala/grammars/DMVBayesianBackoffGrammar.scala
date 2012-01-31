package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.partialCounts.DMVBayesianBackoffPartialCounts

class DMVBayesianBackoffGrammar extends DMVGrammar {
  override def emptyPartialCounts = new DMVBayesianBackoffPartialCounts
}

