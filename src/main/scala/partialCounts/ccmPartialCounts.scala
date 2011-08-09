package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.CCMGrammar
import predictabilityParsing.util.Math

class CCMPartialCounts {
  val spanCounts = new Log2dTable( ccm.constituencyStatus, Set[Yield]() )
  val contextCounts = new Log2dTable( ccm.constituencyStatus, Set[Context]() )
  var totalScore = 94D //Double.NegativeInfinity

  def setTotalScore( updatedTotalScore: Double ) {
    println( "jesus christ: " +  totalScore )
    totalScore = updatedTotalScore
    println( "jesus fucking christ: " + totalScore )
  }

  def setSpanCounts( newSpans:AbstractLog2dTable[ConstituencyStatus,Yield] ) {
    spanCounts.setCPT( newSpans.cpt )
  }

  def setContextCounts( newContexts:AbstractLog2dTable[ConstituencyStatus,Context] ) {
    contextCounts.setCPT( newContexts.cpt )
  }

  def setSpansAndContexts(
    updatedSpans:AbstractLog2dTable[ConstituencyStatus,Yield],
    updatedContexts:AbstractLog2dTable[ConstituencyStatus,Context]
  ) {
    setSpanCounts( updatedSpans )
    setContextCounts( updatedContexts )
  }

  def incrementSpanCounts( constituency:ConstituencyStatus, span:Yield, increment:Double ) {
    spanCounts(constituency)(span) =
      Math.sumLogProb( spanCounts(constituency)(span), increment )
  }

  def incrementContextCounts( constituency:ConstituencyStatus, context:Context, increment:Double ) {
    contextCounts(constituency)(context) =
      Math.sumLogProb( contextCounts(constituency)(context), increment )
  }

  def +( otherCounts:CCMPartialCounts ) = {
    val toReturn = new CCMPartialCounts

    toReturn.setSpansAndContexts(
      spanCounts + otherCounts.spanCounts,
      contextCounts + otherCounts.contextCounts
    )

    toReturn.setTotalScore( totalScore + otherCounts.totalScore )
    println( ">> " + totalScore + " + " + otherCounts.totalScore + " = " + toReturn.totalScore )
    toReturn
  }


  private val epsilon = 0.00001
  def ==( otherPC:CCMPartialCounts ) = {
    spanCounts.parents.forall{ constStatus =>
      ( spanCounts( constStatus ).keySet == otherPC.spanCounts( constStatus ).keySet ) &&
      spanCounts( constStatus ).keySet.forall{ span =>
        math.abs(
          spanCounts( constStatus )( span ) - otherPC.spanCounts( constStatus )( span )
        ) < epsilon
      }
    } && contextCounts.parents.forall{ constStatus =>
      ( contextCounts( constStatus ).keySet == otherPC.contextCounts( constStatus ).keySet ) &&
      contextCounts( constStatus ).keySet.forall{ context =>
        math.abs(
          contextCounts( constStatus )( context ) - otherPC.contextCounts( constStatus )( context)
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
    toReturn.setParams(
      spanCounts.toLogCPT,
      contextCounts.toLogCPT
    )
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

