package predictabilityParsing.parsers
import predictabilityParsing.types.labels._

abstract class AbstractDMVParser {

  protected case class Span( start:Int, end:Int )

  // We need the +1 because our indices are between words.
  protected def oneDistantAdj( w1:MarkedObservation, span:Span ) =
    if( w1.attachmentDirection == RightAttachment )
      span.end - (w1.obs.t+1) <= 1
    else if( w1.attachmentDirection == LeftAttachment )
      w1.obs.t - span.start <= 1
    else
      throw new UnsupportedOperationException( w1 + " takes no attachments" )


  // We need the +1 because our indices are between words.
  protected def adj( w1:MarkedObservation, span:Span ) =
    if( w1.attachmentDirection == RightAttachment )
      (w1.obs.t+1) == span.end
    else if( w1.attachmentDirection == LeftAttachment )
      w1.obs.t == span.start
    else
      throw new UnsupportedOperationException( w1 + " takes no attachments" )

}

