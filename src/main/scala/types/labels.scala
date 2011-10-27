package predictabilityParsing.types.labels

abstract class Label(s:String) {
  override def toString = s

  def <( l2:Label) = toString < l2.toString
  def >( l2:Label) = toString > l2.toString
}

abstract class AttachmentOrder( s:String ) extends HiddenLabel ( s )
object LeftFirst extends AttachmentOrder( "--LeftFirst--" ) 
object RightFirst extends AttachmentOrder( "--RightFirst--" )

abstract class AttachmentStatus( s:String ) extends HiddenLabel ( s ) {
  val attachmentDirection:AttachmentDirection
  val seal:Option[AttachmentStatus]
  val peel:List[AttachmentStatus]
}
object Sealed extends AttachmentStatus( "||" ) {
  val attachmentDirection = NoAttachment
  val seal = None
  val peel = List( SealedLeft, SealedRight )
}
object SealedLeft extends AttachmentStatus( "|>" ) {
  val attachmentDirection = RightAttachment
  val seal = Some(Sealed)
  val peel = UnsealedLeftFirst::Nil
}
object SealedRight extends AttachmentStatus( "<|" ) {
  val attachmentDirection = LeftAttachment
  val seal = Some(Sealed)
  val peel = UnsealedRightFirst::Nil
}
object UnsealedLeftFirst extends AttachmentStatus( "<" ) {
  val attachmentDirection = LeftAttachment
  val seal = Some(SealedLeft)
  val peel = Nil
}
object UnsealedRightFirst extends AttachmentStatus( ">" ) {
  val attachmentDirection = RightAttachment
  val seal = Some(SealedRight)
  val peel = Nil
}

abstract class AttachmentDirection( s:String ) extends HiddenLabel( s )
object NoAttachment extends AttachmentDirection( "--NoAttachment" )
abstract class SomeAttachment( s:String ) extends AttachmentDirection( s )
object LeftAttachment extends SomeAttachment( "--LeftAttachment--" )
object RightAttachment extends SomeAttachment( "--RightAttachment--" )

case class DirectedArc( head:TimedObservedLabel, arg:TimedObservedLabel )
  extends HiddenLabel( head + " --> " + arg )

abstract class StopDecision( s:String ) extends HiddenLabel( s )
object Stop extends StopDecision( "--Stop--" )
object NotStop extends StopDecision( "--NotStop--" )

// Think of adj as the inverse of: Have we attached anything in this direction yet? This way it
// makes sense when sealing a lexical item which has no dependents.
case class StopOrNot( w:ObservedLabel, dir:AttachmentDirection, adj:Boolean )
  extends HiddenLabel( w + ", " + dir + ", " + adj )

case class ChooseArgument( h:ObservedLabel, dir:AttachmentDirection )
  extends HiddenLabel( h + ", " + dir )

case class MarkedObservation( val obs:TimedObservedLabel, val mark:AttachmentStatus )
  extends HiddenLabel( obs.toString + "." + mark  ) {
  val peel = mark.peel.map( MarkedObservation( obs, _ ) )
  lazy val seal = mark.seal.map( MarkedObservation( obs, _ ) )
  val attachmentDirection = mark.attachmentDirection
}

package object dmv {
  val attachmentOrder:Set[AttachmentOrder] = Set( LeftFirst, RightFirst )
  val attachmentStatus:Set[AttachmentStatus] = Set(
    SealedLeft,
    SealedRight,
    UnsealedLeftFirst,
    UnsealedRightFirst,
    Sealed
  )
  val attachmentDirection:Set[AttachmentDirection] = Set( LeftAttachment, RightAttachment )
  val stopDecision:Set[StopDecision] = Set( Stop, NotStop )

  def stopOrNotKeys( vocab:Set[ObservedLabel] ) =
    (vocab + Root).flatMap{ w =>
      attachmentDirection.flatMap{ dir =>
        Set( StopOrNot( w, dir, true ), StopOrNot( w, dir, false ) )
      }
    }

  def chooseKeys( vocab:Set[ObservedLabel] ) =
    (vocab + Root).flatMap{ h =>
      attachmentDirection.map{ dir =>
        ChooseArgument( h, dir )
      }
    }
}

trait StatePair

abstract class ObservedLabel( s:String ) extends Label( s )
abstract class HiddenLabel( s:String ) extends Label( s )


abstract class TimedObservedLabel( val w:ObservedLabel, val t:Int) extends ObservedLabel( w+"."+t ) 
case class TimedWord( s:String, time:Int ) extends TimedObservedLabel( Word( s ), time )

case object Root extends ObservedLabel( "--Root--" )
case object InitialRoot extends TimedObservedLabel( Root, 0 )
case class FinalRoot( n:Int ) extends TimedObservedLabel( Root, n )


abstract class ConstituencyStatus( s:String ) extends HiddenLabel ( s )
object Distituent extends ConstituencyStatus( "--Distituent--" ) 
object Constituent extends ConstituencyStatus( "--Constituent--" )

package object ccm {
  val constituencyStatus:Set[ConstituencyStatus] = Set( Constituent, Distituent )
}


case class Word( w:String ) extends ObservedLabel ( w ) {
  override def hashCode = w.hashCode
}
case class WordPair( val w1:String, val w2:String )
  extends ObservedLabel ( w1 + "^" + w2 ) with StatePair {
  private val asString = w1 + "^" + w2
  override def hashCode = asString.hashCode
  val obsA = Word( w1 )
  val obsB = Word( w2 )
}

object SentenceBoundary extends ObservedLabel( "###" )


case class Yield( y:List[ObservedLabel] )
  extends ObservedLabel( y.mkString( "", " ", "" ) ) {
  private val asString = y.mkString( "", " ", "" )
  override def hashCode = asString.hashCode
}

abstract class AbstractContext( context:String ) extends ObservedLabel( context )

case class RightContext( right:ObservedLabel )
  extends AbstractContext(  "→" + right ) {
  private val asString = ( "→" + right )
  override def hashCode = asString.hashCode
}

case class Context( left:ObservedLabel, right:ObservedLabel )
  extends AbstractContext( "←" + left + "," + "→" + right ) {
  private val asString = ("←" + left + "," + "→" + right)
  override def hashCode = asString.hashCode
}

case class Sentence( sentenceID:String, sentence: List[ObservedLabel] )
  extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )
case class TwoStreamSentence( sentenceID:String, sentence: List[WordPair] )
  extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )
case class TimedSentence( sentenceID:String, sentence: List[TimedObservedLabel] )
  extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

abstract class Parameterization
case class BaseCCM( span:Yield, context:Context ) extends Parameterization
case class TwoContextCCM( span:Yield, contextA:AbstractContext, contextB:AbstractContext ) extends Parameterization


