package predictabilityParsing.types.labels

// trait HashcodeCaching {
//   self: Product => override lazy val hashCode: Int = {
//     //println("hashCode " + this);
//     scala.runtime.ScalaRunTime._hashCode(this)
//   }
// } 

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
case object Distituent extends ConstituencyStatus( "--Distituent--" )
case object Constituent extends ConstituencyStatus( "--Constituent--" )


case class Word( w:String ) extends ObservedLabel ( w ) //with HashcodeCaching
object SentenceBoundary extends ObservedLabel( "###" )// with HashcodeCaching

case class WordPair( s1:String, s2:String ) extends ObservedLabel ( s1 + "^" + s2 ) with StatePair

case class Yield( y:List[ObservedLabel] )
  extends ObservedLabel( y.mkString( "", " ", "" ) ) //with HashcodeCaching

case class Context( left:ObservedLabel, right:ObservedLabel )
  extends ObservedLabel( "←" + left + "," + "→" + right ) //with HashcodeCaching

case class Sentence( sentenceID:String, sentence: List[ObservedLabel] )
  extends Yield( sentence )// with HashcodeCaching

