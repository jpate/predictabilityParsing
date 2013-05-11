package predictabilityParsing.parsers
import predictabilityParsing.grammars.AbstractDMVGrammar
import predictabilityParsing.types.labels._

abstract class AbstractDMVParser {

  protected case class Span( start:Int, end:Int )

  val g:AbstractDMVGrammar
  def setGrammar( givenGrammar:AbstractDMVGrammar ) {
    println( "AbstractDMVParser.setGrammar" )
    g.setParams( givenGrammar.getParams )
  }

  // We need the +1 because our indices are between words.
  protected def adj( w1:MarkedObservation, span:Span ) =
    if( w1.attachmentDirection == RightAttachment )
      (w1.obs.t+1) == span.end
    else if( w1.attachmentDirection == LeftAttachment )
      w1.obs.t == span.start
    else
      throw new UnsupportedOperationException( w1 + " takes no attachments" )

  // We need the +1 because our indices are between words.
  protected def adj( w:TimedObservedLabel, span:Span, dir:AttachmentDirection ) =
    if( dir == RightAttachment )
      (w.t+1) == span.end
    else if( dir == LeftAttachment )
      w.t == span.start
    else
      throw new UnsupportedOperationException( w + " takes no attachments" )


}

