package predictabilityParsing.types.labels

    // trait HashcodeCaching {
    //   // self: Product => override val hashCode: Int = {
    //   //   //scala.runtime.ScalaRunTime._hashCode(this)
    //   //   scala.runtime.ScalaRunTime._hashCode(this)
    //   // }
    // } 

abstract class Label(s:String) {
  override def toString = s

  // override val hashCode = s.hashCode
  // override def equals( other:Any ) = toString == other.toString

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


class Word( w:String ) extends ObservedLabel ( w ) {
  override val hashCode = w.hashCode
  override def equals( other:Any ) = toString == other.toString
}
// {//with HashcodeCaching
//   override val hashCode = w.hashCode
//   override def equals( other:Any ) = toString == other.toString
// }
object SentenceBoundary extends ObservedLabel( "###" ) {
  override val hashCode = "###".hashCode
  override def equals( other:Any ) = toString == other.toString
}

class WordPair( s1:String, s2:String ) extends ObservedLabel ( s1 + "^" + s2 ) with StatePair

class Yield( y:List[ObservedLabel] )
  extends ObservedLabel( y.mkString( "", " ", "" ) )  {
  private val asString = y.mkString( "", " ", "" )
  override val hashCode = asString.hashCode
  override def equals( other:Any ) = toString == other.toString
}

// {
//   override val hashCode = y.hashCode
//   override def equals( other:Any ) = toString == other.toString
// }

class Context( left:ObservedLabel, right:ObservedLabel )
  extends ObservedLabel( "←" + left + "," + "→" + right ) {
  override val hashCode = ("←" + left + "," + "→" + right).hashCode
  override def equals( other:Any ) = toString == other.toString
}
// { //with HashcodeCaching
//   override val hashCode = ("←" + left + "," + "→" + right ).hashCode
//   override def equals( other:Any ) = toString == other.toString
// }

case class Sentence( sentenceID:String, sentence: List[ObservedLabel] )
  extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

