package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.util.Math

class TwoContextCCMGrammar(
  spans:Iterable[Yield],
  contextsA:Iterable[AbstractContext],
  contextsB:Iterable[AbstractContext],
  hallucinatedTrue:Double = 2D,
  hallucinatedFalse:Double = 8D
) extends AbstractCCMGrammar[TwoContextCCM]( hallucinatedTrue, hallucinatedFalse ) {

  private val p_span = new LogCPT( ccm.constituencyStatus, spans )
  private val p_context_a = new LogCPT( ccm.constituencyStatus, contextsA )
  private val p_context_b = new LogCPT( ccm.constituencyStatus, contextsB )

  def phi[TwoContextCCM]( elements:TwoContextCCM ) = {
    val TwoContextCCM( span, contextA, contextB ) = elements
    (
      smoothedSpanScore( Constituent , span ) +
      smoothedContextScoreA( Constituent , contextA ) +
      smoothedContextScoreB( Constituent , contextB )
    ) - (
      smoothedSpanScore( Distituent , span ) +
      smoothedContextScoreA( Distituent , contextA ) +
      smoothedContextScoreB( Distituent , contextB )
    )
  }

  def getPSpan() = p_span
  def getPContextA() = p_context_a
  def getPContextB() = p_context_b

  def setParams( otherGram:TwoContextCCMGrammar ) {
    p_span.setCPT( otherGram.getPSpan /*.getCPT*/ )
    p_context_a.setCPT( otherGram.getPContextA() /*.getCPT*/ )
    p_context_b.setCPT( otherGram.getPContextB() /*.getCPT*/ )
  }

  def setParams(
    updatedSpans:LogCPT[ConstituencyStatus,Yield],
    updatedContextsA:LogCPT[ConstituencyStatus,AbstractContext],
    updatedContextsB:LogCPT[ConstituencyStatus,AbstractContext]
  ) {
    setP_span( updatedSpans )
    setP_contextA( updatedContextsA )
    setP_contextB( updatedContextsB )
  }

  def getSpans = p_span.values.head.keySet
  def getContextsA = p_context_a.values.head.keySet
  def getContextsB = p_context_b.values.head.keySet

  def setP_span( updatedSpans:LogCPT[ConstituencyStatus,Yield] ) {
    p_span.setCPT( updatedSpans /*.cpt*/ )
  }
  def setP_contextA( updatedContextsA:LogCPT[ConstituencyStatus,AbstractContext] ) {
    p_context_a.setCPT( updatedContextsA /*.cpt*/ )
  }
  def setP_contextB( updatedContextsB:LogCPT[ConstituencyStatus,AbstractContext] ) {
    p_context_b.setCPT( updatedContextsB /*.cpt*/ )
  }

  def smoothedSpanScore( constituency:ConstituencyStatus, span:Yield ) =
    constituency match {
      case Constituent => p_span( Constituent ).getOrElse( span, math.log( defaultTrue ) )
      case Distituent => p_span( Distituent ).getOrElse( span, math.log( defaultFalse ) )
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
    p_span.randomize( seed, centeredOn )
    p_context_a.randomize( seed, centeredOn )
    p_context_b.randomize( seed, centeredOn )

    p_span.values.head.keySet.foreach( span =>
      p_span(Distituent)(span) = Math.subtractLogProb( 0D, p_span(Constituent)(span) )
    )
    p_context_a.values.head.keySet.foreach( context_a =>
      p_context_a(Distituent)(context_a) = Math.subtractLogProb( 0D, p_context_a(Constituent)(context_a) )
    )
    p_context_b.values.head.keySet.foreach( context_b =>
      p_context_b(Distituent)(context_b) = Math.subtractLogProb( 0D, p_context_b(Constituent)(context_b) )
    )

    normalize
  }

  def normalize {
    p_span.normalize
    p_context_a.normalize
    p_context_b.normalize
  }

  override def toString =
    "P_Span:\n" +
    "Constituents:" +
    p_span(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span(Constituent)( span ) ) + " (" + p_span(Constituent)( span ) +  ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents:" +
    p_span(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( p_span(Distituent)( span ) ) + " (" + p_span(Distituent)( span ) + ")"
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

