package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.grammars.AbstractDMVGrammar
import predictabilityParsing.util.Math.logSum

class DMVPartialCounts {
  val orderCounts = new Log2dTable( Set[ObservedLabel](), dmv.attachmentOrder )
  val stopCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val chooseCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  def chooseKeys = chooseCounts.parents

  // expDigamma without exp because we are in log-space
  protected def expDigamma( input:Double ) = {
    import math.{exp,log}
    var r = 0D
    var x = exp( input )
    while( x <= 5 ) {
      r -= 1/x
      x += 1
    }
    val f = 1/(x*x)
    val t = f*(-1D/12.0 + f*(1D/120.0 + f*(-1D/252.0 + f*(1/240.0 + f*(-1/132.0
        + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
    r + log(x) - 0.5/x + t;
  }

  protected var totalScore = 0D

  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }
  def incrementTotalScore( increment: Double ) 
    { totalScore = logSum( totalScore, increment) }
  def multiplyTotalScore( multiplicand: Double ) { totalScore += multiplicand }
  def getTotalScore = totalScore

  def setOrderCounts( newOrderCounts:AbstractLog2dTable[ObservedLabel,AttachmentOrder] ) {
    orderCounts.setCPT( newOrderCounts /*.getCPT*/ )
  }

  def setStopCounts( newStopCounts:AbstractLog2dTable[StopOrNot,StopDecision] ) {
    stopCounts.setCPT( newStopCounts /*.getCPT*/ )
  }


  def setChooseCounts( newChooseCounts:AbstractLog2dTable[ChooseArgument,ObservedLabel] ) {
    chooseCounts.setCPT( newChooseCounts /*.getCPT*/ )
  }

  def setParameters(
    updatedOrder:AbstractLog2dTable[ObservedLabel,AttachmentOrder],
    updatedStop:AbstractLog2dTable[StopOrNot,StopDecision],
    updatedChoose:AbstractLog2dTable[ChooseArgument,ObservedLabel]
  ) {
    setOrderCounts( updatedOrder )
    setStopCounts( updatedStop )
    setChooseCounts( updatedChoose )
  }

  def clearInterpolationScores = ()

  def incrementOrderCounts( w:ObservedLabel, order:AttachmentOrder, increment:Double ) {
    orderCounts.setValue(
      w,
      order,
      logSum( orderCounts(w,order), increment )
    )
  }
  def setOrderCounts( w:ObservedLabel, order:AttachmentOrder, newCount:Double ) {
    orderCounts.setValue( w, order, newCount )
  }

  def incrementStopCounts( stopKey:StopOrNot, decision:StopDecision, increment:Double ) {
    stopCounts.setValue(
      stopKey,
      decision,
      logSum( stopCounts(stopKey,decision), increment )
    )
  }
  def setStopCounts( stopKey:StopOrNot, decision:StopDecision, newCount:Double ) {
    stopCounts.setValue( stopKey, decision, newCount )
  }

  def incrementChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, increment:Double ) {
    // println( "\n" + chooseKey + " --> " + arg + ": " + chooseCounts( chooseKey, arg ) )
    // println( "increment " + chooseKey + " --> " + arg + " by " + increment )
    chooseCounts.setValue(
      chooseKey,
      arg,
      logSum( chooseCounts(chooseKey,arg), increment )
    )
    // println( chooseKey + " --> " + arg + ": " + chooseCounts( chooseKey, arg ) + "\n" )
  }
  def setChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, newCount:Double ) {
    chooseCounts.setValue( chooseKey, arg, newCount )
  }

  def +( otherCounts:DMVPartialCounts ) = {
    val toReturn = new DMVPartialCounts


    toReturn.setParameters(
      orderCounts + otherCounts.orderCounts,
      stopCounts + otherCounts.stopCounts,
      chooseCounts + otherCounts.chooseCounts
    )

    toReturn.setTotalScore( totalScore + otherCounts.getTotalScore )
    toReturn
  }

  def divideChooseCounts( x:Double ) {
    chooseCounts.divideBy( x )
  }

  def divideStopCounts( x:Double ) {
    stopCounts.divideBy( x )
  }

  def divideOrderCounts( x:Double ) {
    orderCounts.divideBy( x )
  }

  def normalizeChooseCounts {
    chooseCounts.normalize
  }

  def getChooseCountsString = "chooseCounts:\n" + chooseCounts.toString



  def destructivePlus( otherCounts:DMVPartialCounts ) {
    //println( "regular destructivePlus" )
    val otherP_data = otherCounts.getTotalScore

    otherCounts.orderCounts.parents.foreach{ w =>
      incrementOrderCounts( w , LeftFirst , otherCounts.orderCounts( w, LeftFirst ) )
      incrementOrderCounts( w , RightFirst , otherCounts.orderCounts( w, RightFirst ) )
    }

    otherCounts.stopCounts.parents.foreach{ stopKey =>
      incrementStopCounts( stopKey , Stop , otherCounts.stopCounts( stopKey , Stop ) )
      incrementStopCounts( stopKey , NotStop , otherCounts.stopCounts( stopKey , NotStop ) )
    }


    otherCounts.chooseCounts.parents.foreach{ chooseKey =>
      otherCounts.chooseCounts(chooseKey).keySet.foreach{ w =>
        w match {
          case rootArg:AbstractRoot => { /* Intentionally empty */ }
          case _ =>
            incrementChooseCounts(
              chooseKey,
              w,
              otherCounts.chooseCounts( chooseKey , w )
            )
        }
      }
    }

    multiplyTotalScore( otherCounts.getTotalScore )
  }


  def associatedGrammar:AbstractDMVGrammar = new DMVGrammar//( orderCounts.parents.toSet )
  /*
   * For now, we just normalize. In the future, we can sum up the denominator and pass through a
   * digamma function (or something) for variational bayes.
   *
   */
  def toDMVGrammar( posteriorMode:Boolean = false, posteriorMean:Boolean = false )  = {
    // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    val toReturn = associatedGrammar

    //println( "DMVPartialCounts.toDMVGrammar" )

    // println( "StopCounts:\n" + stopCounts + "\n\n -- END STOP COUNTS ---\n\n" )
    // println( "ChooseCounts:\n" + chooseCounts + "\n\n -- END CHOOSE COUNTS ---\n\n" )

    toReturn.setParams(
      VanillaDMVParameters(
        orderCounts.toLogCPT,
        stopCounts.toLogCPT,
        chooseCounts.toLogCPT
      )
    )

    toReturn.normalize
    toReturn
  }


  def toLaplaceSmoothedGrammar[O<:ObservedLabel](vocab:Set[O], smooth:Double ) = {
    val logSmooth = math.log( smooth )

    val rightFirstValue = orderCounts( orderCounts.parents.head, RightFirst )
    val leftFirstValue = orderCounts( orderCounts.parents.head, LeftFirst )

    vocab.foreach{ w =>
      orderCounts.setValue(
        w,
        RightFirst,
        rightFirstValue
      )
      orderCounts.setValue(
        w,
        LeftFirst,
        leftFirstValue
      )
    }

    ( stopCounts.parents ++ dmv.rootlessStopOrNotKeys( vocab ) ).foreach{ key =>
      dmv.stopDecision.foreach{ decision =>
        stopCounts.setValue(
          key,
          decision,
          logSum(
            stopCounts( key, decision ),
            logSmooth
          )
        )
      }
    }

    ( chooseCounts.parents ++ dmv.rootlessChooseKeys( vocab ) ).foreach{ key =>
      vocab.foreach{ arg =>
        chooseCounts.setValue(
          key,
          arg,
          logSum(
            chooseCounts( key, arg ),
            logSmooth
          )
        )
      }
    }

    toDMVGrammar()
  }

  def toVariationalDMVGrammar(
    stopAlpha:Double = 1D,
    chooseAlpha:Double = 1D,
    posteriorMode:Boolean = false,
    posteriorMean:Boolean = false
  ) = {
    // val toReturn = new DMVGrammar( orderCounts.parents.toSet )
    val toReturn = associatedGrammar

    //println( "StopCounts:\n" + stopCounts + "\n\n -- END STOP COUNTS ---\n\n" )

    //orderCounts.expDigammaNormalize(partialCounts)
    orderCounts.normalize
    if( posteriorMode )
      stopCounts.posteriorModeNormalize(stopAlpha, alphaUnk=false)
    else if( posteriorMean )
      stopCounts.posteriorMeanNormalize(stopAlpha, alphaUnk=false)
    else
      stopCounts.expDigammaNormalize(stopAlpha, alphaUnk=false)

    stopCounts.setValue(
      StopOrNot( Root, RightAttachment, true ),
      Stop,
      0D
    )
    stopCounts.setValue(
      StopOrNot( Root, RightAttachment, true ),
      NotStop,
      Double.NegativeInfinity
    )
    stopCounts.setValue(
      StopOrNot( Root, RightAttachment, false ),
      Stop,
      0D
    )
    stopCounts.setValue(
      StopOrNot( Root, RightAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )

    stopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      NotStop,
      0D
    )
    stopCounts.setValue(
      StopOrNot( Root, LeftAttachment, true ),
      Stop,
      Double.NegativeInfinity
    )

    stopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      NotStop,
      Double.NegativeInfinity
    )
    stopCounts.setValue(
      StopOrNot( Root, LeftAttachment, false ),
      Stop,
      0D
    )


    if( posteriorMode )
      chooseCounts.posteriorModeNormalize(chooseAlpha)
    else if( posteriorMean )
      chooseCounts.posteriorMeanNormalize(chooseAlpha)
    else
      chooseCounts.expDigammaNormalize(chooseAlpha)

    toReturn.setParams(
      VanillaDMVParameters(
        orderCounts.asLogCPT,
        stopCounts.asLogCPT,
        chooseCounts.asLogCPT
      )
    )

    //toReturn.normalize
    toReturn
  }

  override def toString =
    //"orderCounts:\n" + orderCounts +
    "chooseCounts:\n" + chooseCounts +
    "stopCounts:\n" + stopCounts

  def chooseCSV =
    chooseCounts.parents.flatMap{ head =>
      chooseCounts( head ).keySet.map{ dep =>
        head.h + "," + head.dir + "," + dep + "," + chooseCounts(head,dep)
      }
    }

  def stopCSV =
    stopCounts.parents.flatMap{ head =>
      Array(
        head.w + "," + head.dir + "," + head.adj + "," + Stop + "," + stopCounts(head,Stop) ,
        head.w + "," + head.dir + "," + head.adj + "," + NotStop + "," + stopCounts(head,NotStop)
      )
    }

  // override def toString =
  //   "Span Counts:\n" +
  //   "Constituents:" +
  //   spanCounts(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
  //     span + " ==> " + math.exp( spanCounts(Constituent)( span ) )
  //   }.mkString("\n\t","\n\t","\n\n") +
  //   "Distituents:" +
  //   spanCounts(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ span =>
  //     span + " ==> " + math.exp( spanCounts(Distituent)( span ) )
  //   }.mkString("\n\t","\n\t","\n\n") +
  //   "Context Counts:\n" +
  //   "Constituents" +
  //   contextCounts(Constituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
  //     context + " ==> " + math.exp( contextCounts(Constituent)( context ) )
  //   }.mkString("\n\t","\n\t","\n\n") +
  //   "Distituents" +
  //   contextCounts(Distituent).keySet.toList.sortWith( (a,b) => a < b ).map{ context =>
  //     context + " ==> " + math.exp( contextCounts(Distituent)( context ) )
  //   }.mkString("\n\t","\n\t","\n\n")
}

