package predictabilityParsing.types.labels
import predictabilityParsing.types.tables._

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
  val sealCount:Int
}
object Sealed extends AttachmentStatus( "||" ) {
  val attachmentDirection = NoAttachment
  val seal = None
  val peel = List( SealedLeft, SealedRight )
  val sealCount = 2
}
object SealedLeft extends AttachmentStatus( "|>" ) {
  val attachmentDirection = RightAttachment
  val seal = Some(Sealed)
  val peel = UnsealedLeftFirst::Nil
  val sealCount = 1
}
object SealedRight extends AttachmentStatus( "<|" ) {
  val attachmentDirection = LeftAttachment
  val seal = Some(Sealed)
  val peel = UnsealedRightFirst::Nil
  val sealCount = 1
}
object UnsealedLeftFirst extends AttachmentStatus( "<" ) {
  val attachmentDirection = LeftAttachment
  val seal = Some(SealedLeft)
  val peel = Nil
  val sealCount = 0
}
object UnsealedRightFirst extends AttachmentStatus( ">" ) {
  val attachmentDirection = RightAttachment
  val seal = Some(SealedRight)
  val peel = Nil
  val sealCount = 0
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

abstract class BackoffDecision( s:String ) extends HiddenLabel( s )
object Backoff extends BackoffDecision( "--Backoff--" )
object BackoffOne extends BackoffDecision( "--BackoffOne--" )
object BackoffTwo extends BackoffDecision( "--BackoffTwo--" )
object BackoffArg extends BackoffDecision( "--BackoffArg--" )
object BackoffHead extends BackoffDecision( "--BackoffHead--" )
object BackoffBoth extends BackoffDecision( "--BackoffBoth--" )
object NotBackoff extends BackoffDecision( "--NotBackoff--" )

// Think of adj as the inverse of: Have we attached anything in this direction yet? This way it
// makes sense when sealing a lexical item which has no dependents.
case class StopOrNot( w:ObservedLabel, dir:AttachmentDirection, adj:Boolean )
  extends HiddenLabel( w + ", " + dir + ", " + adj )


abstract class ChooseKey( h:ObservedLabel, dir:AttachmentDirection )
  extends HiddenLabel( h + ", " + dir )

case class ChooseArgument( h:ObservedLabel, dir:AttachmentDirection )
  extends ChooseKey( h, dir )

case class TwoStreamHeadChooseArg( h:WordPair, dir:AttachmentDirection )
  extends ChooseKey( h, dir )

case class MarkedObservationPair( val obs:TimedObservedLabelPair, val mark:AttachmentStatus )
  extends HiddenLabel( obs.toString + "." + mark  ) {
  val peel = mark.peel.map( MarkedObservationPair( obs, _ ) )
  lazy val seal = mark.seal.map( MarkedObservationPair( obs, _ ) )
  val attachmentDirection = mark.attachmentDirection
}

case class MarkedObservation( val obs:TimedObservedLabel, val mark:AttachmentStatus )
  extends HiddenLabel( obs.toString + "." + mark  ) {
  val peel = mark.peel.map( MarkedObservation( obs, _ ) )
  lazy val seal = mark.seal.map( MarkedObservation( obs, _ ) )
  val attachmentDirection = mark.attachmentDirection
  val sealCount = mark.sealCount
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
  val backoffDecision:Set[BackoffDecision] = Set( Backoff, NotBackoff )
  val threeStreamBackoffDecision:Set[BackoffDecision] = Set( NotBackoff, BackoffOne, BackoffTwo )

  val chooseBackoffDecision:Set[BackoffDecision] = Set(
    NotBackoff,
    BackoffHead,
    BackoffArg,
    BackoffBoth
  )

  // def stopOrNotKeys( vocab:Set[_<:ObservedLabel] ) =
  //   (vocab + Root).flatMap{ w =>
  //     attachmentDirection.flatMap{ dir =>
  //       Set( StopOrNot( w, dir, true ), StopOrNot( w, dir, false ) )
  //     }
  //   }

  def rootlessStopOrNotKeys( vocab:Set[_<:ObservedLabel] ) =
    vocab.flatMap{ w =>
      attachmentDirection.flatMap{ dir =>
        Set( StopOrNot( w, dir, true ), StopOrNot( w, dir, false ) )
      }
    }

  // def chooseKeys( vocab:Set[_<:ObservedLabel] ) =
  //   (vocab + Root).flatMap{ h =>
  //     attachmentDirection.map{ dir =>
  //       ChooseArgument( h, dir )
  //     }
  //   }

  def rootlessChooseKeys( vocab:Set[_<:ObservedLabel] ) =
    vocab.flatMap{ h =>
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

abstract class TimedObservedLabelPair( val wp:ObservedLabelPair, t:Int )
  extends TimedObservedLabel( wp, t )
  //extends ObservedLabelPair( wp.obsA+"."+t, wp.obsB+"."+t )

abstract class TimedObservedLabelTriple( val wt:ObservedLabelTriple, t:Int )
  extends TimedObservedLabel( wt, t )
  //extends ObservedLabelPair( wp.obsA+"."+t, wp.obsB+"."+t )


case class TimedWordPair( w1:String, w2:String, time:Int ) 
  extends TimedObservedLabelPair( WordPair( w1,w2 ), time )

case class TimedWordTriple( w1:String, w2:String, w3:String, time:Int ) 
  extends TimedObservedLabelTriple( WordTriple( w1,w2,w3 ), time )

trait RootState
abstract class AbstractRoot extends ObservedLabel( "--Root--" )
// case object Root extends ObservedLabel( "--Root--" ) with RootState
case object Root extends AbstractRoot with RootState
case object InitialRoot extends TimedObservedLabel( Root, 0 ) with RootState
case class FinalRoot( n:Int ) extends TimedObservedLabel( Root, n ) with RootState


abstract class ConstituencyStatus( s:String ) extends HiddenLabel ( s )
object Distituent extends ConstituencyStatus( "--Distituent--" ) 
object Constituent extends ConstituencyStatus( "--Constituent--" )

package object ccm {
  val constituencyStatus:Set[ConstituencyStatus] = Set( Constituent, Distituent )
}


case class Word( w:String ) extends ObservedLabel ( w ) {
  override val hashCode = w.hashCode
}

abstract class ObservedLabelPair( w1:String, w2:String )
  extends ObservedLabel ( w1 + "^" + w2 ) with StatePair {
  // private val asString = w1 + "^" + w2
  // override val hashCode = asString.hashCode
  val obsA = Word( w1 )
  val obsB = Word( w2 )
}
abstract class ObservedLabelTriple( w1:String, w2:String, w3:String )
  extends ObservedLabel ( w1 + "^" + w2 + "^" + w3 ) {
  // private val asString = w1 + "^" + w2 + "^" + w3
  // override val hashCode = asString.hashCode
  val obsA = Word( w1 )
  val obsB = Word( w2 )
  val obsC = Word( w3 )
}
abstract class ObservedLabelQuad( w1:String, w2:String, w3:String, w4:String )
  extends ObservedLabel ( w1 + "^" + w2 + "^" + w3 + "^" + w4 ) {
  // private val asString = w1 + "^" + w2 + "^" + w3 + "^" + w4
  // override val hashCode = asString.hashCode
  val obsA = Word( w1 )
  val obsB = Word( w2 )
  val obsC = Word( w3 )
  val obsD = Word( w4 )
}
abstract class ObservedLabelSextuple( w1:String, w2:String, w3:String, w4:String, w5:String,
w6:String )
  extends ObservedLabel ( w1 + "^" + w2 + "^" + w3 + "^" + w4 + "^" + w5 + "^" + w6 ) {
  // private val asString = w1 + "^" + w2 + "^" + w3 + "^" + w4
  // override val hashCode = asString.hashCode
  val obsA = Word( w1 )
  val obsB = Word( w2 )
  val obsC = Word( w3 )
  val obsD = Word( w4 )
  val obsE = Word( w5 )
  val obsF = Word( w6 )
}

case class WordPair( w1:String, w2:String ) extends ObservedLabelPair( w1, w2 ) {
  override val hashCode = (w1+"^"+w2).hashCode
}
case class WordTriple( w1:String, w2:String, w3:String ) extends ObservedLabelTriple( w1, w2, w3 ) {
  override val hashCode = (w1+"^"+w2+"^"+w3).hashCode
}
case class WordQuad( w1:String, w2:String, w3:String, w4:String )
  extends ObservedLabelQuad( w1, w2, w3, w4 ) {
  //override val hashCode = (w1+"^"+w2+"^"+w3+"^"+w4).hashCode
}
case class WordSextuple( w1:String, w2:String, w3:String, w4:String, w5:String, w6:String )
  extends ObservedLabelSextuple( w1, w2, w3, w4, w5, w6 ) {
  //override val hashCode = (w1+"^"+w2+"^"+w3+"^"+w4).hashCode
}
case object RootPair extends ObservedLabelPair( "--Root--", "--Root--" )

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


abstract class AbstractSentence[O<:ObservedLabel]( sentenceID:String, sentence:List[O] )

trait AbstractTimedSentence

case class Sentence( sentenceID:String, sentence:List[ObservedLabel] )
  extends AbstractSentence( sentenceID, sentence )
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

case class TwoStreamSentence( sentenceID:String, sentence: List[WordPair] )
  extends AbstractSentence( sentenceID, sentence )
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

case class ThreeStreamSentence( sentenceID:String, sentence: List[WordTriple] )
  extends AbstractSentence( sentenceID, sentence )
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

case class TimedSentence( sentenceID:String, sentence: List[TimedObservedLabel] )
  extends AbstractSentence( sentenceID, sentence ) with AbstractTimedSentence
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

case class TimedTwoStreamSentence( sentenceID:String, sentence: List[TimedObservedLabelPair] )
  extends AbstractSentence( sentenceID, sentence ) with AbstractTimedSentence
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

case class TimedThreeStreamSentence( sentenceID:String, sentence: List[TimedObservedLabelTriple] )
  extends AbstractSentence( sentenceID, sentence ) with AbstractTimedSentence
  //extends ObservedLabel( sentenceID + ": " + sentence.mkString(""," ","" ) )

abstract class Parameterization
case class BaseCCM( span:Yield, context:Context ) extends Parameterization
case class TwoContextCCM( span:Yield, contextA:AbstractContext, contextB:AbstractContext ) extends Parameterization
case class TwoContextTwoSpanCCM(
  spanA:Yield,
  spanB:Yield,
  contextA:AbstractContext,
  contextB:AbstractContext
) extends Parameterization

abstract class DMVParameters extends Parameterization

case class VanillaDMVParameters(
  otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
  otherP_stop:LogCPT[StopOrNot,StopDecision],
  otherP_choose:LogCPT[ChooseArgument,ObservedLabel]
) extends DMVParameters

    // case class DMVBayesianBackoffParameters(
    //   //freeEnergy:Double,
    //   otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
    //   otherP_stop:LogCPT[StopOrNot,StopDecision],
    //   otherP_choose:LogCPT[ChooseArgument,ObservedLabel],
    //   stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
    //   chooseBackoffScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
    // ) extends DMVParameters

case class DMVBayesianBackoffParameters(
  //freeEnergy:Double,
  stopBackoffInterpolationScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  stopNoBackoffScore:AbstractLog2dTable[StopOrNot,StopDecision],
  stopBackoffScore:AbstractLog2dTable[StopOrNot,StopDecision],
  chooseBackoffHeadInterpolationScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  noBackoffHeadScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  backoffHeadScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  rootChooseCounts:AbstractLog2dTable[ChooseArgument,ObservedLabel]
) extends DMVParameters

case class DMVBayesianBackoffIndependentDepsParameters(
  //freeEnergy:Double,
  stopBackoffInterpolationScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  stopNoBackoffScore:AbstractLog2dTable[StopOrNot,StopDecision],
  stopBackoffScore:AbstractLog2dTable[StopOrNot,StopDecision],
  chooseBackoffHeadInterpolationScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  noBackoffHeadAScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  noBackoffHeadBScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  backoffHeadAScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  backoffHeadBScore:AbstractLog2dTable[ChooseArgument,ObservedLabel],
  rootChooseCounts:AbstractLog2dTable[ChooseArgument,ObservedLabel]
) extends DMVParameters

case class DMVFullBayesianBackoffParameters(
  //freeEnergy:Double,
  otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
  otherP_stop:LogCPT[StopOrNot,StopDecision],
  otherP_choose:LogCPT[ChooseArgument,ObservedLabel],
  stopBackoffScore:AbstractLog2dTable[StopOrNot,BackoffDecision],
  backoffHeadScore:AbstractLog2dTable[ChooseArgument,BackoffDecision],
  backoffArgScore:AbstractLog2dTable[ChooseArgument,BackoffDecision]
) extends DMVParameters


case class DMVBayesianBackoffSimpleThreeStreamParameters(
  p_order:LogCPT[ObservedLabel,AttachmentOrder],
  p_stop:LogCPT[StopOrNot,StopDecision],
  p_choose:LogCPT[ChooseArgument,ObservedLabel],
  noStopBackoff_Score:AbstractLog1dTable[StopOrNot],
  stopBackoffW_Score:AbstractLog1dTable[StopOrNot],
  stopBackoffA_Score:AbstractLog1dTable[StopOrNot],
  stopBackoffWA_Score:AbstractLog1dTable[StopOrNot],
  stopBackoffPA_Score:AbstractLog1dTable[StopOrNot],
  noChooseBackoff_Score:AbstractLog1dTable[ChooseArgument],
  chooseBackoffW_Score:AbstractLog1dTable[ChooseArgument],
  chooseBackoffA_Score:AbstractLog1dTable[ChooseArgument],
  chooseBackoffWA_Score:AbstractLog1dTable[ChooseArgument],
  chooseBackoffPA_Score:AbstractLog1dTable[ChooseArgument]
) extends DMVParameters

case class DMVIndependentStreamHeadsParameters(
  otherP_order:LogCPT[ObservedLabel,AttachmentOrder],
  otherP_stop:LogCPT[StopOrNot,StopDecision],
  otherP_chooseA:LogCPT[ChooseArgument,ObservedLabel],
  otherP_chooseB:LogCPT[ChooseArgument,ObservedLabel]
) extends DMVParameters

