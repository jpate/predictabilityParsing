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
case object Distituent extends ConstituencyStatus( "--Distituent--" )
case object Constituent extends ConstituencyStatus( "--Constituent--" )


case class Word( w:String ) extends ObservedLabel ( w )
case object SentenceBoundary extends ObservedLabel( "###" )

case class WordPair( s1:String, s2:String ) extends ObservedLabel ( s1 + "^" + s2 ) with StatePair

case class Yield( y:List[ObservedLabel] ) extends ObservedLabel( y.mkString( "", " ", "" ) )

case class Context( left:ObservedLabel, right:ObservedLabel )
  extends ObservedLabel( "←" + left + "," + "→" + right )


