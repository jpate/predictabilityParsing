package predictabilityParsing.partialCounts

import predictabilityParsing.types.labels._
import predictabilityParsing.types.tables._
import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.util.Math

class DMVPartialCounts {
  val orderCounts = new Log2dTable( Set[ObservedLabel](), dmv.attachmentOrder )
  val stopCounts = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
  val stopDenomCounts = new Log1dTable( Set[StopOrNot]() )
  var chooseCounts = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )

  private var totalScore = 0D

  def setTotalScore( updatedTotalScore: Double ) { totalScore = updatedTotalScore }
  def incrementTotalScore( increment: Double ) 
    { totalScore = Math.sumLogProb( totalScore, increment) }
  def multiplyTotalScore( multiplicand: Double ) { totalScore += multiplicand }
  def getTotalScore = totalScore

  def setOrderCounts( newOrderCounts:AbstractLog2dTable[ObservedLabel,AttachmentOrder] ) {
    orderCounts.setCPT( newOrderCounts.getCPT )
  }

  def setStopCounts( newStopCounts:AbstractLog2dTable[StopOrNot,StopDecision] ) {
    stopCounts.setCPT( newStopCounts.getCPT )
  }
  def setStopDenomCounts( newStopDenomCounts:AbstractLog1dTable[StopOrNot] ) {
    stopDenomCounts.setPT( newStopDenomCounts.getPT )
  }

  def setChooseCounts( newChooseCounts:AbstractLog2dTable[ChooseArgument,ObservedLabel] ) {
    chooseCounts.setCPT( newChooseCounts.getCPT )
  }

  def setParameters(
    updatedOrder:AbstractLog2dTable[ObservedLabel,AttachmentOrder],
    updatedStop:AbstractLog2dTable[StopOrNot,StopDecision],
    updatedStopDenom:AbstractLog1dTable[StopOrNot],
    updatedChoose:AbstractLog2dTable[ChooseArgument,ObservedLabel]
  ) {
    setOrderCounts( updatedOrder )
    setStopCounts( updatedStop )
    setStopDenomCounts( updatedStopDenom )
    setChooseCounts( updatedChoose )
  }


  def incrementOrderCounts( w:ObservedLabel, order:AttachmentOrder, increment:Double ) {
    orderCounts.setValue(
      w,
      order,
      Math.sumLogProb( orderCounts(w,order), increment )
    )
  }
  def setOrderCounts( w:ObservedLabel, order:AttachmentOrder, newCount:Double ) {
    orderCounts.setValue( w, order, newCount )
  }

  def incrementStopCounts( stopKey:StopOrNot, decision:StopDecision, increment:Double ) {
    stopCounts.setValue(
      stopKey,
      decision,
      Math.sumLogProb( stopCounts(stopKey,decision), increment )
    )
  }
  def setStopCounts( stopKey:StopOrNot, decision:StopDecision, newCount:Double ) {
    stopCounts.setValue( stopKey, decision, newCount )
  }
  def incrementStopDenomCounts( stopKey:StopOrNot, increment:Double ) {
    stopDenomCounts.setValue(
      stopKey,
      Math.sumLogProb( stopDenomCounts(stopKey), increment )
    )
  }
  def setStopDenomCounts( stopKey:StopOrNot, newCount:Double ) {
    stopDenomCounts.setValue( stopKey, newCount )
  }

  def incrementChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, increment:Double ) {
    chooseCounts.setValue(
      chooseKey,
      arg,
      Math.sumLogProb( chooseCounts(chooseKey,arg), increment )
    )
  }
  def setChooseCounts( chooseKey:ChooseArgument, arg:ObservedLabel, newCount:Double ) {
    chooseCounts.setValue( chooseKey, arg, newCount )
  }


  def +( otherCounts:DMVPartialCounts ) = {
    val toReturn = new DMVPartialCounts


    toReturn.setParameters(
      orderCounts + otherCounts.orderCounts,
      stopCounts + otherCounts.stopCounts,
      stopDenomCounts + otherCounts.stopDenomCounts,
      chooseCounts + otherCounts.chooseCounts
    )

    toReturn.setTotalScore( totalScore + otherCounts.getTotalScore )
    toReturn
  }

  def destructivePlus( otherCounts:DMVPartialCounts ) {
    otherCounts.orderCounts.parents.foreach{ w =>
      incrementOrderCounts( w , LeftFirst , otherCounts.orderCounts( w , LeftFirst ) )
      incrementOrderCounts( w , RightFirst , otherCounts.orderCounts( w , RightFirst ) )
    }
    otherCounts.stopCounts.parents.foreach{ stopKey =>
      incrementStopCounts( stopKey , Stop , otherCounts.stopCounts( stopKey , Stop ) )
      incrementStopDenomCounts( stopKey , otherCounts.stopDenomCounts( stopKey ) )
    }
    otherCounts.chooseCounts.parents.foreach{ chooseKey =>
      otherCounts.chooseCounts(chooseKey).keySet.foreach{ w =>
        incrementChooseCounts(
          chooseKey,
          w,
          otherCounts.chooseCounts( chooseKey , w )
        )
      }
    }

    multiplyTotalScore( otherCounts.getTotalScore )
  }


  /*
   * For now, we just normalize. In the future, we can sum up the denominator and pass through a
   * digamma function (or something) for variational bayes.
   *
   */
  def toDMVGrammar = {
    val toReturn = new DMVGrammar( orderCounts.parents.toSet )


    stopCounts.parents.foreach{ stopKey =>
      val p_stop = 
        stopCounts(stopKey)(Stop) - stopDenomCounts(stopKey)
      val p_not_stop =
        Math.subtractLogProb(0D , stopCounts(stopKey)(Stop) - stopDenomCounts(stopKey) )
      stopCounts.setValue(
        stopKey,
        Stop,
        p_stop
      )
      stopCounts.setValue(
        stopKey,
        NotStop,
        p_not_stop
      )
    }

    println( "CHOOSE COUNTS\n\n" )
    println( chooseCounts )

    println( "\n\n\n------------\np_choose" )
    println( chooseCounts.toLogCPT )
    println( "\n\nDONE FROM TOGRAMMAR" )

    toReturn.setParams(
      orderCounts.toLogCPT,
      stopCounts.toLogCPT,
      chooseCounts.toLogCPT
    )

    toReturn.normalize
    toReturn
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

