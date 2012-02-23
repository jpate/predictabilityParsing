package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.TwoContextTwoSpanCCMGrammar
import predictabilityParsing.util.Math

class TwoContextTwoSpanCCMPartialCounts( val smoothTrue:Double = 2D, val smoothFalse:Double = 8D ) {
  private val spanCountsA = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  private val spanCountsB = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  private val contextCountsA = new Log2dTable( ccm.constituencyStatus, Set[AbstractContext]() )
  private val contextCountsB = new Log2dTable( ccm.constituencyStatus, Set[AbstractContext]() )
  private var totalScore = 0D //Initialize to probability of 1 since we typically multiply this

  // TODO: factor this out
  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }
  def incrementTotalScore( increment: Double ) 
    { totalScore = Math.sumLogProb( totalScore, increment) }
  def multiplyTotalScore( multiplicand: Double ) { totalScore += multiplicand }
  def getTotalScore = totalScore

  def setSpanCountsA( newSpansA:AbstractLog2dTable[ConstituencyStatus,Yield] ) {
    spanCountsA.setCPT( newSpansA /*.getCPT*/ )
  }
  def setSpanCountsB( newSpansB:AbstractLog2dTable[ConstituencyStatus,Yield] ) {
    spanCountsB.setCPT( newSpansB /*.getCPT*/ )
  }

  def setContextCountsA( newContextsA:AbstractLog2dTable[ConstituencyStatus,AbstractContext] ) {
    contextCountsA.setCPT( newContextsA /*.getCPT*/ )
  }
  def setContextCountsB( newContextsB:AbstractLog2dTable[ConstituencyStatus,AbstractContext] ) {
    contextCountsB.setCPT( newContextsB /*.getCPT*/ )
  }

  def getSpanCountsA( constituency:ConstituencyStatus, span:Yield ) =
    spanCountsA( constituency ).getOrElse( span , Double.NegativeInfinity )
  def getSpanCountsB( constituency:ConstituencyStatus, span:Yield ) =
    spanCountsB( constituency ).getOrElse( span , Double.NegativeInfinity )

  def getContextCountsA( constituency:ConstituencyStatus, contextA:AbstractContext ) =
    contextCountsA( constituency ).getOrElse( contextA , Double.NegativeInfinity )
  def getContextCountsB( constituency:ConstituencyStatus, contextB:AbstractContext ) =
    contextCountsB( constituency ).getOrElse( contextB , Double.NegativeInfinity )

  def getSpansA( constituency:ConstituencyStatus ) = spanCountsA( constituency ).keySet
  def getSpansB( constituency:ConstituencyStatus ) = spanCountsB( constituency ).keySet
  def getContextsA( constituency:ConstituencyStatus ) = contextCountsA( constituency ).keySet
  def getContextsB( constituency:ConstituencyStatus ) = contextCountsB( constituency ).keySet

  def getSpansA = spanCountsA( Constituent ).keySet
  def getSpansB = spanCountsB( Constituent ).keySet
  def getContextsA = contextCountsA( Constituent ).keySet
  def getContextsB = contextCountsB( Constituent ).keySet

  def incrementSpanCountsA( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCountsA.setValue(
      constituency,
      span,
      Math.sumLogProb( getSpanCountsA(constituency, span), increment )
    )
  }
  def incrementSpanCountsB( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCountsB.setValue(
      constituency,
      span,
      Math.sumLogProb( getSpanCountsA(constituency, span), increment )
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

  def setSpanCountA( constituency:ConstituencyStatus, span:Yield, newCount:Double ) {
    spanCountsA.setValue( constituency, span, newCount )
  }
  def setSpanCountB( constituency:ConstituencyStatus, span:Yield, newCount:Double ) {
    spanCountsB.setValue( constituency, span, newCount )
  }
  def setContextCountA( constituency:ConstituencyStatus, contextA:AbstractContext, newCount:Double ) {
    contextCountsA.setValue( constituency, contextA, newCount )
  }
  def setContextCountB( constituency:ConstituencyStatus, contextB:AbstractContext, newCount:Double ) {
    contextCountsB.setValue( constituency, contextB, newCount )
  }

  def setSpansAndContexts(
    updatedSpansA:AbstractLog2dTable[ConstituencyStatus,Yield],
    updatedSpansB:AbstractLog2dTable[ConstituencyStatus,Yield],
    updatedContextsA:AbstractLog2dTable[ConstituencyStatus,AbstractContext],
    updatedContextsB:AbstractLog2dTable[ConstituencyStatus,AbstractContext]
  ) {
    setSpanCountsA( updatedSpansA )
    setSpanCountsB( updatedSpansB )
    setContextCountsA( updatedContextsA )
    setContextCountsB( updatedContextsB )
  }

  def +( otherCounts:TwoContextTwoSpanCCMPartialCounts ) = {
    val toReturn = new TwoContextTwoSpanCCMPartialCounts( smoothTrue, smoothFalse )

    toReturn.setSpansAndContexts(
      spanCountsA + otherCounts.spanCountsA,
      spanCountsB + otherCounts.spanCountsB,
      contextCountsA + otherCounts.contextCountsA,
      contextCountsB + otherCounts.contextCountsB
    )

    toReturn.setTotalScore( totalScore + otherCounts.getTotalScore )
    toReturn
  }

  def destructivePlus( otherCounts:TwoContextTwoSpanCCMPartialCounts ) {
    otherCounts.getSpansA.foreach{ span =>
      incrementSpanCountsA( Constituent, span, otherCounts.getSpanCountsA( Constituent, span ) )
      incrementSpanCountsA( Distituent, span, otherCounts.getSpanCountsA( Distituent, span ) )
    }

    otherCounts.getSpansB.foreach{ span =>
      incrementSpanCountsB( Constituent, span, otherCounts.getSpanCountsB( Constituent, span ) )
      incrementSpanCountsB( Distituent, span, otherCounts.getSpanCountsB( Distituent, span ) )
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

  // made private because arguments are in log-space
  private def hallucinateCounts( hallucinateTrue:Double, hallucinateFalse:Double ) {
    getSpansA.foreach{ span =>
      incrementSpanCountsA( Constituent, span, hallucinateTrue )
      incrementSpanCountsA( Distituent, span, hallucinateFalse )
    }
    getSpansB.foreach{ span =>
      incrementSpanCountsB( Constituent, span, hallucinateTrue )
      incrementSpanCountsB( Distituent, span, hallucinateFalse )
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

  def toTwoContextTwoSpanCCMGrammar:TwoContextTwoSpanCCMGrammar =
    toTwoContextTwoSpanCCMGrammar( smoothTrue , smoothFalse )

  def toTwoContextTwoSpanCCMGrammar( hallucinateTrue:Double, hallucinateFalse:Double ) = {
    val toReturn = new TwoContextTwoSpanCCMGrammar(
      spanCountsA.values.head.keySet /*children*/,
      spanCountsB.values.head.keySet /*children*/,
      contextCountsA.values.head.keySet /*children*/,
      contextCountsB.values.head.keySet /*children*/,
      hallucinateTrue,
      hallucinateFalse
    )


    hallucinateCounts( math.log( hallucinateTrue ), math.log( hallucinateFalse ) )

    val p_span_a = spanCountsA.toLogCPT
    val p_span_b = spanCountsB.toLogCPT
    val p_context_a = contextCountsA.toLogCPT
    val p_context_b = contextCountsB.toLogCPT

    toReturn.setParams(
      p_span_a,
      p_span_b,
      p_context_a,
      p_context_b
    )

    toReturn.normalize
    toReturn
  }

}

