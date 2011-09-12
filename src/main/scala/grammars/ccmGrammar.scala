package predictabilityParsing.grammars

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._

class CCMGrammar( spans:Iterable[Yield], contexts:Iterable[Context] ) {

  val p_span = new LogCPT( ccm.constituencyStatus, spans )
  val p_context = new LogCPT( ccm.constituencyStatus, contexts )

  def spanScore( constituency:ConstituencyStatus, span:Yield ) =
    p_span( constituency ).getOrElse( span , Double.NegativeInfinity )
  def contextScore( constituency:ConstituencyStatus, context:Context ) =
    p_context( constituency ).getOrElse( context , Double.NegativeInfinity )

  def setP_span( updatedSpans:LogCPT[ConstituencyStatus,Yield] ) {
    p_span.setCPT( updatedSpans.cpt )
  }
  def setP_context( updatedContexts:LogCPT[ConstituencyStatus,Context] ) {
    p_context.setCPT( updatedContexts.cpt )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    p_span.randomize( seed, centeredOn )
    p_context.randomize( seed, centeredOn )
  }
  def randomize( seed:Int ) {
    randomize( seed, 0 )
  }

  def setParams(
    updatedSpans:LogCPT[ConstituencyStatus,Yield],
    updatedContexts:LogCPT[ConstituencyStatus,Context]
  ) {
    setP_span( updatedSpans )
    setP_context( updatedContexts )
  }

  def normalize {
    p_span.normalize
    p_context.normalize
  }

  private val epsilon = 0.00001
  def ==( otherGram:CCMGrammar ) = {
    p_span.parents.forall{ constStatus =>
      ( p_span( constStatus ).keySet == otherGram.p_span( constStatus ).keySet ) &&
      p_span( constStatus ).keySet.forall{ span =>
        math.abs(
          p_span( constStatus )( span ) - otherGram.p_span( constStatus )( span )
        ) < epsilon
      }
    } && p_context.parents.forall{ constStatus =>
      ( p_context( constStatus ).keySet == otherGram.p_context( constStatus ).keySet ) &&
      p_context( constStatus ).keySet.forall{ context =>
        math.abs(
          p_context( constStatus )( context ) - otherGram.p_context( constStatus )( context)
        ) < epsilon
      }
    }
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
    "P_Context:\n" +
    "Constituents" +
    p_context(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
      context + " ==> " + math.exp( p_context(Constituent)( context ) ) + " (" + p_context(Constituent)( context )+ ")"
    }.mkString("\n\t","\n\t","\n\n") +
    "Distituents" +
    p_context(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
      context + " ==> " + math.exp( p_context(Distituent)( context ) ) + " (" + p_context(Distituent)( context )+ ")"
    }.mkString("\n\t","\n\t","\n\n")


}

