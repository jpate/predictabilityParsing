package predictabilityParsing.types.labels

abstract class Label(s:String) {
  override def toString = s

  def <( l2:Label) = toString < l2.toString
  def >( l2:Label) = toString > l2.toString
}

package object ccm {
  val constituencyStatus:Set[ConstituencyStatus] = Set( Constituent, Distituent )
}

trait StatePair

abstract class ObservedLabel( s:String ) extends Label( s )
abstract class HiddenLabel( s:String ) extends Label( s )

abstract class ConstituencyStatus( s:String ) extends HiddenLabel ( s )
object Distituent extends ConstituencyStatus( "--Distituent--" ) 
object Constituent extends ConstituencyStatus( "--Constituent--" )


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


