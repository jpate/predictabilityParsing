package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.TwoContextCCMGrammar
import predictabilityParsing.util.Math

class TwoContextCCMPartialCounts( val smoothTrue:Double = 2D, val smoothFalse:Double = 8D ) {
  private val spanCounts = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  private val contextCountsA = new Log2dTable( ccm.constituencyStatus, Set[AbstractContext]() )
  private val contextCountsB = new Log2dTable( ccm.constituencyStatus, Set[AbstractContext]() )
  private var totalScore = 0D //Initialize to probability of 1 since we typically multiply this

  // TODO: factor this out
  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }
  def incrementTotalScore( increment: Double ) 
    { totalScore = Math.sumLogProb( totalScore, increment) }
  def multiplyTotalScore( multiplicand: Double ) { totalScore += multiplicand }
  def getTotalScore = totalScore

  def setSpanCounts( newSpans:AbstractLog2dTable[ConstituencyStatus,Yield] ) {
    spanCounts.setCPT( newSpans.getCPT )
  }

  def setContextCountsA( newContextsA:AbstractLog2dTable[ConstituencyStatus,AbstractContext] ) {
    contextCountsA.setCPT( newContextsA.getCPT )
  }
  def setContextCountsB( newContextsB:AbstractLog2dTable[ConstituencyStatus,AbstractContext] ) {
    contextCountsB.setCPT( newContextsB.getCPT )
  }

  def getSpanCounts( constituency:ConstituencyStatus, span:Yield ) =
    spanCounts( constituency ).getOrElse( span , Double.NegativeInfinity )
  def getContextCountsA( constituency:ConstituencyStatus, contextA:AbstractContext ) =
    contextCountsA( constituency ).getOrElse( contextA , Double.NegativeInfinity )
  def getContextCountsB( constituency:ConstituencyStatus, contextB:AbstractContext ) =
    contextCountsB( constituency ).getOrElse( contextB , Double.NegativeInfinity )

  def getSpans( constituency:ConstituencyStatus ) = spanCounts( constituency ).keySet
  def getContextsA( constituency:ConstituencyStatus ) = contextCountsA( constituency ).keySet
  def getContextsB( constituency:ConstituencyStatus ) = contextCountsB( constituency ).keySet

  def getSpans = spanCounts( Constituent ).keySet
  def getContextsA = contextCountsA( Constituent ).keySet
  def getContextsB = contextCountsB( Constituent ).keySet

  def incrementSpanCounts( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCounts.setValue(
      constituency,
      span,
      Math.sumLogProb( getSpanCounts(constituency, span), increment )
    )
  }
  def incrementContextCountsA( constituency:ConstituencyStatus, contextA:AbstractContext, increment:Double ) {
    contextCountsA.setValue(
      constituency,
      contextA,
      Math.sumLogProb( getContextCountsA(constituency, contextA), increment )
    )
  }
  def incrementContextCountsB( constituency:ConstituencyStatus, contextB:AbstractContext, increment:Double ) {
    contextCountsB.setValue(
      constituency,
      contextB,
      Math.sumLogProb( getContextCountsB(constituency, contextB), increment )
    )
  }

  def setSpanCount( constituency:ConstituencyStatus, span:Yield, newCount:Double ) {
    spanCounts.setValue( constituency, span, newCount )
  }
  def setContextCountA( constituency:ConstituencyStatus, contextA:AbstractContext, newCount:Double ) {
    contextCountsA.setValue( constituency, contextA, newCount )
  }
  def setContextCountB( constituency:ConstituencyStatus, contextB:AbstractContext, newCount:Double ) {
    contextCountsB.setValue( constituency, contextB, newCount )
  }

  def setSpansAndContexts(
    updatedSpans:AbstractLog2dTable[ConstituencyStatus,Yield],
    updatedContextsA:AbstractLog2dTable[ConstituencyStatus,AbstractContext],
    updatedContextsB:AbstractLog2dTable[ConstituencyStatus,AbstractContext]
  ) {
    setSpanCounts( updatedSpans )
    setContextCountsA( updatedContextsA )
    setContextCountsB( updatedContextsB )
  }

  def +( otherCounts:TwoContextCCMPartialCounts ) = {
    val toReturn = new TwoContextCCMPartialCounts( smoothTrue, smoothFalse )

    toReturn.setSpansAndContexts(
      spanCounts + otherCounts.spanCounts,
      contextCountsA + otherCounts.contextCountsA,
      contextCountsB + otherCounts.contextCountsB
    )

    toReturn.setTotalScore( totalScore + otherCounts.getTotalScore )
    toReturn
  }

  def destructivePlus( otherCounts:TwoContextCCMPartialCounts ) {
    otherCounts.getSpans.foreach{ span =>
      incrementSpanCounts( Constituent, span, otherCounts.getSpanCounts( Constituent, span ) )
      incrementSpanCounts( Distituent, span, otherCounts.getSpanCounts( Distituent, span ) )
    }

    otherCounts.getContextsA.foreach{ contextA =>
      incrementContextCountsA( Constituent, contextA, otherCounts.getContextCountsA( Constituent,
      contextA ) )
      incrementContextCountsA( Distituent, contextA, otherCounts.getContextCountsA( Distituent,
      contextA ) )
    }

    otherCounts.getContextsB.foreach{ contextB =>
      incrementContextCountsB( Constituent, contextB, otherCounts.getContextCountsB( Constituent,
      contextB ) )
      incrementContextCountsB( Distituent, contextB, otherCounts.getContextCountsB( Distituent,
      contextB ) )
    }

    multiplyTotalScore( otherCounts.getTotalScore )
  }

  private def hallucinateCounts( hallucinateTrue:Double, hallucinateFalse:Double ) {
    getSpans.foreach{ span =>
      incrementSpanCounts( Constituent, span, hallucinateTrue )
      incrementSpanCounts( Distituent, span, hallucinateFalse )
    }
    getContextsA.foreach{ contextA =>
      incrementContextCountsA( Constituent, contextA, hallucinateTrue )
      incrementContextCountsA( Distituent, contextA, hallucinateFalse )
    }
    getContextsB.foreach{ contextB =>
      incrementContextCountsB( Constituent, contextB, hallucinateTrue )
      incrementContextCountsB( Distituent, contextB, hallucinateFalse )
    }
  }

  def toTwoContextCCMGrammar:TwoContextCCMGrammar =
    toTwoContextCCMGrammar( smoothTrue, smoothFalse )

  def toTwoContextCCMGrammar( hallucinateTrue:Double, hallucinateFalse:Double ) = {
    val toReturn = new TwoContextCCMGrammar(
      spanCounts.children,
      contextCountsA.children,
      contextCountsB.children,
      hallucinateTrue,
      hallucinateFalse
    )


    hallucinateCounts( math.log( hallucinateTrue ), math.log( hallucinateFalse ) )

    val p_span = spanCounts.toLogCPT
    val p_context_a = contextCountsA.toLogCPT
    val p_context_b = contextCountsB.toLogCPT

    toReturn.setParams(
      p_span,
      p_context_a,
      p_context_b
    )

    toReturn.normalize
    toReturn
  }

}

