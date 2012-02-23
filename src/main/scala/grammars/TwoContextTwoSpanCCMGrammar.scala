package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

class TwoContextTwoSpanCCMGrammar(
  spansA:Iterable[Yield],
  spansB:Iterable[Yield],
  contextsA:Iterable[AbstractContext],
  contextsB:Iterable[AbstractContext],
  hallucinatedTrue:Double = 2D,
  hallucinatedFalse:Double = 8D
) extends AbstractCCMGrammar[TwoContextTwoSpanCCM]( hallucinatedTrue, hallucinatedFalse ) {

  private val p_span_a = new LogCPT( ccm.constituencyStatus, spansA )
  private val p_span_b = new LogCPT( ccm.constituencyStatus, spansB )
  private val p_context_a = new LogCPT( ccm.constituencyStatus, contextsA )
  private val p_context_b = new LogCPT( ccm.constituencyStatus, contextsB )

  def phi[TwoContextTwoSpanCCM]( elements:TwoContextTwoSpanCCM ) = {
    val TwoContextTwoSpanCCM( spanA, spanB, contextA, contextB ) = elements
    (
      smoothedSpanScoreA( Constituent , spanA ) +
      smoothedSpanScoreB( Constituent , spanB ) +
      smoothedContextScoreA( Constituent , contextA ) +
      smoothedContextScoreB( Constituent , contextB )
    ) - (
      smoothedSpanScoreA( Distituent , spanA ) +
      smoothedSpanScoreB( Distituent , spanB ) +
      smoothedContextScoreA( Distituent , contextA ) +
      smoothedContextScoreB( Distituent , contextB )
    )
  }

  def getPSpanA() = p_span_a
  def getPSpanB() = p_span_b
  def getPContextA() = p_context_a
  def getPContextB() = p_context_b

  def setParams( otherGram:TwoContextTwoSpanCCMGrammar ) {
    p_span_a.setCPT( otherGram.getPSpanA /*.getCPT*/ )
    p_span_b.setCPT( otherGram.getPSpanB /*.getCPT*/ )
    p_context_a.setCPT( otherGram.getPContextA() /*.getCPT*/ )
    p_context_b.setCPT( otherGram.getPContextB() /*.getCPT*/ )
  }

  def setParams(
    updatedSpansA:LogCPT[ConstituencyStatus,Yield],
    updatedSpansB:LogCPT[ConstituencyStatus,Yield],
    updatedContextsA:LogCPT[ConstituencyStatus,AbstractContext],
    updatedContextsB:LogCPT[ConstituencyStatus,AbstractContext]
  ) {
    setP_spanA( updatedSpansA )
    setP_spanB( updatedSpansB )
    setP_contextA( updatedContextsA )
    setP_contextB( updatedContextsB )
  }

  def getSpansA = p_span_a.values.head.keySet /*children*/
  def getSpansB = p_span_b.values.head.keySet /*children*/
  def getContextsA = p_context_a.values.head.keySet /*children*/
  def getContextsB = p_context_b.values.head.keySet /*children*/

  def setP_spanA( updatedSpans:LogCPT[ConstituencyStatus,Yield] ) {
    p_span_a.setCPT( updatedSpans /*.cpt*/ )
  }
  def setP_spanB( updatedSpans:LogCPT[ConstituencyStatus,Yield] ) {
    p_span_b.setCPT( updatedSpans /*.cpt*/ )
  }
  def setP_contextA( updatedContextsA:LogCPT[ConstituencyStatus,AbstractContext] ) {
    p_context_a.setCPT( updatedContextsA /*.cpt*/ )
  }
  def setP_contextB( updatedContextsB:LogCPT[ConstituencyStatus,AbstractContext] ) {
    p_context_b.setCPT( updatedContextsB /*.cpt*/ )
  }

  def smoothedSpanScoreA( constituency:ConstituencyStatus, span:Yield ) =
    constituency match {
      case Constituent => p_span_a( Constituent ).getOrElse( span, math.log( defaultTrue ) )
      case Distituent => p_span_a( Distituent ).getOrElse( span, math.log( defaultFalse ) )
    }
  def smoothedSpanScoreB( constituency:ConstituencyStatus, span:Yield ) =
    constituency match {
      case Constituent => p_span_b( Constituent ).getOrElse( span, math.log( defaultTrue ) )
      case Distituent => p_span_b( Distituent ).getOrElse( span, math.log( defaultFalse ) )
    }

  def smoothedContextScoreA( constituency:ConstituencyStatus, context:AbstractContext ) =
    constituency match {
      case Constituent => p_context_a( Constituent ).getOrElse( context, math.log( defaultTrue ) )
      case Distituent => p_context_a( Distituent ).getOrElse( context, math.log( defaultFalse ) )
    }

  def smoothedContextScoreB( constituency:ConstituencyStatus, context:AbstractContext ) =
    constituency match {
      case Constituent => p_context_b( Constituent ).getOrElse( context, math.log( defaultTrue ) )
      case Distituent => p_context_b( Distituent ).getOrElse( context, math.log( defaultFalse ) )
    }

  def randomize( seed:Int, centeredOn:Int ) {
    p_span_a.randomize( seed, centeredOn )
    p_span_b.randomize( seed, centeredOn )
    p_context_a.randomize( seed, centeredOn )
    p_context_b.randomize( seed, centeredOn )

    p_span_a.values.head.keySet /*children*/.foreach( span =>
      p_span_a(Distituent)(span) = Math.subtractLogProb( 0D, p_span_a(Constituent)(span) )
    )
    p_span_b.values.head.keySet /*children*/.foreach( span =>
      p_span_b(Distituent)(span) = Math.subtractLogProb( 0D, p_span_b(Constituent)(span) )
    )
    p_context_a.values.head.keySet /*children*/.foreach( context_a =>
      p_context_a(Distituent)(context_a) = Math.subtractLogProb( 0D, p_context_a(Constituent)(context_a) )
    )
    p_context_b.values.head.keySet /*children*/.foreach( context_b =>
      p_context_b(Distituent)(context_b) = Math.subtractLogProb( 0D, p_context_b(Constituent)(context_b) )
    )

    normalize
  }

  def normalize {
    p_span_a.normalize
    p_span_b.normalize
    p_context_a.normalize
    p_context_b.normalize
  }

  override def toString =
    "P_SpanA:\n" +
    "Constituents:" +
    p_span_a(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span_a(Constituent)( span ) ) + " (" + p_span_a(Constituent)( span ) +  ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents:" +
    p_span_a(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span_a(Distituent)( span ) ) + " (" + p_span_a(Distituent)( span ) + ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "P_SpanB:\n" +
    "Constituents:" +
    p_span_b(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span_b(Constituent)( span ) ) + " (" + p_span_b(Constituent)( span ) +  ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents:" +
    p_span_a(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span_b(Distituent)( span ) ) + " (" + p_span_b(Distituent)( span ) + ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "P_ContextA:\n" +
    "Constituents" +
    p_context_a(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context_a =>
      context_a + " ==> " + math.exp( p_context_a(Constituent)( context_a ) ) + " (" + p_context_a(Constituent)( context_a )+ ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents" +
    p_context_a(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context_a =>
      context_a + " ==> " + math.exp( p_context_a(Distituent)( context_a ) ) + " (" + p_context_a(Distituent)( context_a )+ ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "P_ContextB:\n" +
    "Constituents" +
    p_context_b(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context_b =>
      context_b + " ==> " + math.exp( p_context_b(Constituent)( context_b ) ) + " (" + p_context_b(Constituent)( context_b )+ ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents" +
    p_context_b(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context_b =>
      context_b + " ==> " + math.exp( p_context_b(Distituent)( context_b ) ) + " (" + p_context_b(Distituent)( context_b )+ ")"
    }.mkString("\n\t","\n\t","\n\n")

}

