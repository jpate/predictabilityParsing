package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.Math

class CCMPartialCounts {
  private val spanCounts = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  private val contextCounts = new Log2dTable( ccm.constituencyStatus, Set[Context]() )
  var totalScore = 0D //Double.NegativeInfinity

  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }

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

  def getSpans( constituency:ConstituencyStatus ) = spanCounts( constituency ).keySet
  def getContexts( constituency:ConstituencyStatus ) = contextCounts( constituency ).keySet

  def getSpans = spanCounts( Constituent ).keySet
  def getContexts = contextCounts( Constituent ).keySet

  def incrementSpanCounts( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCounts(constituency)(span) =
      Math.sumLogProb( getSpanCounts(constituency, span), increment )
  }
  def setSpanCount( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCounts(constituency)(span) = increment
  }

  def incrementContextCounts( constituency:ConstituencyStatus, context:Context, increment:Double ) {
    contextCounts(constituency)(context) =
      Math.sumLogProb( getContextCounts(constituency, context), increment )
  }
  def setContextCount( constituency:ConstituencyStatus, context:Context, increment:Double ) {
    contextCounts(constituency)(context) = increment
  }

  // def divideSpanCounts( divisor:Double ) { spanCounts.divideBy( divisor ) }
  // def divideContextCounts( divisor:Double ) { contextCounts.divideBy( divisor ) }

  def +( otherCounts:CCMPartialCounts ) = {
    val toReturn = new CCMPartialCounts

    toReturn.setSpansAndContexts(
      spanCounts + otherCounts.spanCounts,
      contextCounts + otherCounts.contextCounts
    )

    toReturn.setTotalScore( totalScore + otherCounts.totalScore )
    toReturn
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

  /*
   * For now, we just normalize. In the future, we can sum up the denominator and pass through a
   * digamma function for variational bayes.
   *
   */
  def toCCMGrammar = {
    val toReturn = new CCMGrammar( spanCounts.children, contextCounts.children )

    /*
    spanCounts.hallucinateCounts(
      collection.mutable.Map[ConstituencyStatus,Double](
        Constituent -> math.log( 0.2 ), Distituent -> math.log( 0.8 )
      )
    )

    contextCounts.hallucinateCounts(
      collection.mutable.Map[ConstituencyStatus,Double](
        Constituent -> math.log( 0.2 ), Distituent -> math.log( 0.8 )
      )
    )
    */


    val p_span = spanCounts.toLogCPT
    val p_context = contextCounts.toLogCPT

    // OK, this is a hack, but it seems to do the right thing with the probability of the data...
    p_span(Constituent).keySet.foreach{ span =>
      p_span(Distituent)(span) = math.log( 1D - math.exp( p_span(Constituent)(span) ) )
    }
    p_context(Constituent).keySet.foreach{ context =>
      p_context(Distituent)(context) = math.log( 1D - math.exp( p_context(Constituent)(context) ) )
    }

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

