package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.Math

class CCMPartialCounts( val smoothTrue:Double = 2D, val smoothFalse:Double = 8D ) {
  private val spanCounts = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  private val contextCounts = new Log2dTable( ccm.constituencyStatus, Set[Context]() )
  private var totalScore = 0D //Initialize to probability of 1 since we typically multiply this

  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }
  def incrementTotalScore( increment: Double ) 
    { totalScore = Math.sumLogProb( totalScore, increment) }
  def multiplyTotalScore( multiplicand: Double ) { totalScore += multiplicand }
  def getTotalScore = totalScore

  def setSpanCounts( newSpans:AbstractLog2dTable[ConstituencyStatus,Yield] ) {
    spanCounts.setCPT( newSpans.getCPT )
  }

  def setContextCounts( newContexts:AbstractLog2dTable[ConstituencyStatus,Context] ) {
    contextCounts.setCPT( newContexts.getCPT )
  }

  def setSpansAndContexts(
    updatedSpans:AbstractLog2dTable[ConstituencyStatus,Yield],
    updatedContexts:AbstractLog2dTable[ConstituencyStatus,Context]
  ) {
    setSpanCounts( updatedSpans )
    setContextCounts( updatedContexts )
  }

  def getSpanCounts( constituency:ConstituencyStatus, span:Yield ) =
    spanCounts( constituency ).getOrElse( span , Double.NegativeInfinity )
  def getContextCounts( constituency:ConstituencyStatus, context:Context ) =
    contextCounts( constituency ).getOrElse( context , Double.NegativeInfinity )

  def getSpanCounts() = spanCounts
  def getContextCounts() = contextCounts

  def getSpans( constituency:ConstituencyStatus ) = spanCounts( constituency ).keySet
  def getContexts( constituency:ConstituencyStatus ) = contextCounts( constituency ).keySet

  def getSpans = spanCounts( Constituent ).keySet
  def getContexts = contextCounts( Constituent ).keySet

  def incrementSpanCounts( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCounts.setValue(
      constituency,
      span,
      Math.sumLogProb( getSpanCounts(constituency, span), increment )
    )
  }
  def setSpanCount( constituency:ConstituencyStatus, span:Yield, newCount:Double ) {
    spanCounts.setValue( constituency, span, newCount )
  }

  def incrementContextCounts( constituency:ConstituencyStatus, context:Context, increment:Double ) {
    contextCounts.setValue(
      constituency,
      context,
      Math.sumLogProb( getContextCounts(constituency, context), increment )
    )
  }
  def setContextCount( constituency:ConstituencyStatus, context:Context, newCount:Double ) {
    contextCounts.setValue( constituency, context, newCount )
  }

  def divideSpanCounts( divisorMap:collection.immutable.Map[ConstituencyStatus,Double] )
    { spanCounts.divideBy( divisorMap ) }
  def divideContextCounts( divisorMap:collection.immutable.Map[ConstituencyStatus,Double] )
    { contextCounts.divideBy( divisorMap ) }

  def +( otherCounts:CCMPartialCounts ) = {
    val toReturn = new CCMPartialCounts

    toReturn.setSpansAndContexts(
      spanCounts + otherCounts.spanCounts,
      contextCounts + otherCounts.contextCounts
    )

    toReturn.setTotalScore( totalScore + otherCounts.getTotalScore )
    toReturn
  }

  def destructivePlus( otherCounts:CCMPartialCounts ) {
    otherCounts.getSpans.foreach{ span =>
      incrementSpanCounts( Constituent, span, otherCounts.getSpanCounts( Constituent, span ) )
      incrementSpanCounts( Distituent, span, otherCounts.getSpanCounts( Distituent, span ) )
    }

    otherCounts.getContexts.foreach{ context =>
      incrementContextCounts( Constituent, context, otherCounts.getContextCounts( Constituent, context ) )
      incrementContextCounts( Distituent, context, otherCounts.getContextCounts( Distituent, context ) )
    }

    multiplyTotalScore( otherCounts.getTotalScore )
  }

  private val epsilon = 0.00001
  def ==( otherPC:CCMPartialCounts ) = {
    spanCounts.parents.forall{ constStatus =>
      ( spanCounts( constStatus ).keySet == otherPC.spanCounts( constStatus ).keySet ) &&
      spanCounts( constStatus ).keySet.forall{ span =>
        math.abs(
          getSpanCounts( constStatus , span ) - otherPC.getSpanCounts( constStatus , span )
        ) < epsilon
      }
    } && contextCounts.parents.forall{ constStatus =>
      ( contextCounts( constStatus ).keySet == otherPC.contextCounts( constStatus ).keySet ) &&
      contextCounts( constStatus ).keySet.forall{ context =>
        math.abs(
          getContextCounts( constStatus , context ) - otherPC.getContextCounts( constStatus , context)
        ) < epsilon
      }
    }
  }

  def hallucinateCounts( hallucinateTrue:Double, hallucinateFalse:Double ) {
    getSpans.foreach{ span =>
      incrementSpanCounts( Constituent, span, hallucinateTrue )
      incrementSpanCounts( Distituent, span, hallucinateFalse )
    }
    getContexts.foreach{ context =>
      incrementContextCounts( Constituent, context, hallucinateTrue )
      incrementContextCounts( Distituent, context, hallucinateFalse )
    }
  }

  // quick and easy/dirty default
  def toCCMGrammar:CCMGrammar = toCCMGrammar( math.log( smoothTrue ), math.log( smoothFalse ) )

  /*
   * For now, we just normalize. In the future, we can sum up the denominator and pass through a
   * digamma function for variational bayes.
   *
   */
  def toCCMGrammar( hallucinateTrue:Double, hallucinateFalse:Double ) = {
    val toReturn = new CCMGrammar( spanCounts.children, contextCounts.children )


    hallucinateCounts( hallucinateTrue, hallucinateFalse )

    val p_span = spanCounts.toLogCPT
    val p_context = contextCounts.toLogCPT

    toReturn.setParams(
      p_span,
      p_context
    )

    toReturn.normalize
    toReturn
  }

  override def toString =
    "Span Counts:\n" +
    "Constituents:" +
    spanCounts(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( spanCounts(Constituent)( span ) )
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents:" +
    spanCounts(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
      span + " ==> " + math.exp( spanCounts(Distituent)( span ) )
    }.mkString("\n\t","\n\t","\n\n") +
    "Context Counts:\n" +
    "Constituents" +
    contextCounts(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
      context + " ==> " + math.exp( contextCounts(Constituent)( context ) )
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents" +
    contextCounts(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
      context + " ==> " + math.exp( contextCounts(Distituent)( context ) )
    }.mkString("\n\t","\n\t","\n\n")
}

