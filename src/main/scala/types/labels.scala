package predictabilityParsing.types.labels

abstract class Label(s:String) {
  override def toString = s

  def <( l2:Label) = toString < l2.toString
  def >( l2:Label) = toString > l2.toString
}

abstract class AttachmentOrder( s:String ) extends HiddenLabel ( s )
object LeftFirst extends AttachmentOrder( "--LeftFirst--" ) 
object RightFirst extends AttachmentOrder( "--RightFirst--" )

abstract class AttachmentStatus( s:String ) extends HiddenLabel ( s )
object Sealed extends AttachmentStatus( "--Sealed--" )
object SealedLeft extends AttachmentStatus( "--SealedLeft--" )
object SealedRight extends AttachmentStatus( "--SealedRight--" )
object UnsealedLeftFirst extends AttachmentStatus( "--UnsealedLeftFirst--" )
object UnsealedRightFirst extends AttachmentStatus( "--UnsealedRightFirst--" )

abstract class AttachmentDirection( s:String ) extends HiddenLabel( s )
object LeftAttachment extends AttachmentDirection( "--LeftAttachment--" )
object RightAttachment extends AttachmentDirection( "--RightAttachment--" )

abstract class StopAttaching( s:String ) extends HiddenLabel( s )
object Stop extends StopAttaching( "--Stop--" )
object NotStop extends StopAttaching( "--NotStop--" )

// Think of adj as: Have we attached anything in this direction yet? This way it makes sense when
// sealing a lexical item which has no dependents.
case class StopOrNot( w:ObservedLabel, dir:AttachmentDirection, adj:Bool )
  extends HiddenLabel( w + ", " + dir + ", " + adj )

case class ChooseArgument( h:ObservedLabel, dir:AttachmentDirection )
  extends HiddenLabel( h + ", " + dir )

case class MarkedObservation( val obs:TimedObservedLabel, val mark:AttachmentStatus )
  extends HiddenLabel( obs + "-" + mark )

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
  val stopAttaching:Set[StopAttaching] = Set( Stop, NotStop )

  def stopOrNotKeys( vocab:Iterable[ObservedLabel] ) {
    vocab.flatMap{ w =>
      attachmentDirection.flatMap{ dir =>
        Set( StopOrNot( w, dir, true ), StopOrNot( w, dir, false ) )
      }
    }
  }

  def chooseKeys( vocab:Iterable[ObservedLabel] ) {
    vocab.flatMap{ h =>
      attachmentDirection.flatMap{ dir =>
        ChooseArgument( h, dir )
      }
    }
  }
}

trait StatePair

abstract class ObservedLabel( s:String ) extends Label( s )
abstract class HiddenLabel( s:String ) extends Label( s )


abstract class TimedObservedLabel( s:String, t:Int) extends ObservedLabel( s+".".t ) {
}
case class TimedWord( val w:String, t:Int ) extends TimedObservedLabel( w, t )

case class Root( t:Int ) extends TimedObservedLabel( "--Root--", t )


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

abstract class Parameterization
case class BaseCCM( span:Yield, context:Context ) extends Parameterization
case class TwoContextCCM( span:Yield, contextA:AbstractContext, contextB:AbstractContext ) extends Parameterization


