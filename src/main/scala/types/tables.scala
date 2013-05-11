package predictabilityParsing.types.tables

// import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math.{expDigamma,subtractLogProb,logSum}
import scala.collection.mutable.Map
import math.{exp,log}

/*
 * Basic properties for a table.
 */
abstract class AbstractTable {
  def randomize(seed:Int,centeredOn:Int):Unit
  def randomize( seed:Int ) { randomize( seed, 0 ) }
  def normalize:Unit

  private var defaultVal = Double.NegativeInfinity
  def getDefault = defaultVal
  def setDefault( x:Double ) { defaultVal = x }
}

/*
 * Basic properties for a 2-dimensional table (with rows and columns)
 * TODO factor out the dirichlet specific bits
 */
abstract class AbstractLog2dTable[T<:Label,U<:Label]
  extends AbstractTable {

  protected var cpt:Map[T,Map[U,Double]]


  def values = cpt.values
  //def apply( k:T ) = cpt.getOrElse( k, Map[U,Double]().withDefaultValue( getDefault ) )
  def apply( k:T ) = cpt.getOrElse( k, defaultChildMap )
  def apply( parent:T, child:U ) =
    cpt.getOrElse(
      parent,
      defaultChildMap
    ).getOrElse(
      child,
      defaultParentMap.getOrElse( parent, super.getDefault )
    )
    // cpt( parent )( child )
    // cpt.getOrElse(
    //   parent,
    //   defaultParentMap( parent )
    // ).getOrElse(
    //   child,
    //   defaultChildMap( child )
    // )

  def clear = {
    // setCPT(
    //   new Log2dTable( Set[T](), Set[U]() )
    // )
    cpt = Map[T,Map[U,Double]]()
  }

  def definedAt( parent:T, child:U ) = cpt.isDefinedAt(parent) && cpt(parent).isDefinedAt(child)

  def delete( parent:T, child:U ) {
    cpt(parent) -= child
  }

  def getParentDefault( k:T ):Double = defaultParentMap.getOrElse( k, super.getDefault )
  def getChildDefault( k:U ):Double = defaultChildMap.getOrElse( k, super.getDefault )

  def getDefaultParentMap = defaultParentMap
  def getDefaultChildMap = defaultChildMap

  private var defaultParentMap = collection.immutable.Map[T,Double]().withDefaultValue( super.getDefault )
  private var defaultChildMap = collection.immutable.Map[U,Double]().withDefaultValue( super.getDefault )

  def setDefaultParentMap( newDefaultParentMap:collection.mutable.Map[T,Double] ) {
    //defaultParentMap.clear
    defaultParentMap = newDefaultParentMap.toMap
  }
  def setDefaultParentMap( newDefaultParentMap:collection.immutable.Map[T,Double] ) {
    //defaultParentMap.clear
    defaultParentMap = newDefaultParentMap
  }

  def setDefaultChildMap( newDefaultChildMap:collection.mutable.Map[U,Double] ) {
    //defaultChildMap.clear
    defaultChildMap = newDefaultChildMap.toMap
  }
  def setDefaultChildMap( newDefaultChildMap:collection.immutable.Map[U,Double] ) {
    //defaultChildMap.clear
    defaultChildMap = newDefaultChildMap
  }
  def setDefaultChildMap( newDefaultChildMap:collection.Map[U,Double] ) {
    //defaultChildMap.clear
    defaultChildMap = newDefaultChildMap.toMap
  }

  def setCPTMap( updatedCPT: Map[T,Map[U,Double]] ) {
    cpt = updatedCPT
  }

  //def setCPT( updatedCPT: AbstractLog2dTable[T,U] ) {
  def setCPT( updatedCPT: AbstractLog2dTable[T,U] ) {
    cpt = updatedCPT.getCPT
    setDefault( updatedCPT.getDefault )
    setDefaultParentMap( updatedCPT.getDefaultParentMap )
    setDefaultChildMap( updatedCPT.getDefaultChildMap )
  }

  def setValue( parent:T, child:U, newValue:Double ) {
    //cpt( parent )( child ) = newValue
    //cpt = cpt ++ Map( parent -> Map( child -> newValue ) )
    if( cpt.keySet.contains( parent ) )
      cpt(parent) += child -> newValue
    else
      cpt += parent -> Map( child -> newValue )
  }

  def normalize {
    val maxes = Map(
      cpt.keySet.map( parent =>
        if( cpt( parent ).values.size > 0 )
          //parent -> ( cpt(parent).values.reduce( Math.sumLogProb(_,_) ) )
          parent -> logSum( cpt(parent).values.toSeq )
        else
          parent -> Double.NegativeInfinity
          //parent -> 0D
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> ( this(parent, child) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )
  }

  def posteriorMeanNormalize( pseudoCount:Double = 1D, alphaUnk:Boolean = true ) {
    val logPseudoCount = log( pseudoCount )
    val maxes = Map(
      cpt.keySet.map( parent => {
          val childCount = cpt( parent ).values.size
          if( childCount > 0 )
            parent -> logSum(
              log( pseudoCount*( childCount + { if( alphaUnk ) 1D else 0D } ) )::cpt(parent).values.toList
            )
          else
            parent -> Double.NegativeInfinity
            //parent -> 0D
        }
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> ( logSum( this(parent, child), logPseudoCount ) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    if( alphaUnk )
      setDefaultParentMap(
        Map(
          cpt.keySet.map{ parent =>
            parent -> { logPseudoCount - maxes( parent ) }
          }.toSeq:_*
        )
      )
  }

  def posteriorMeanNormalize( pseudoCountMap:scala.collection.Map[U,Double] ) {
    val logPseudoCountMap = pseudoCountMap.mapValues( log( _ ) )
    val maxes = Map(
      cpt.keySet.map( parent => {
          val childCount = cpt( parent ).values.size
          if( childCount > 0 )
            parent ->
              logSum( log(pseudoCountMap.values.reduce(_+_))::cpt(parent).values.toList )
          else
            parent -> Double.NegativeInfinity
        }
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> (
                logSum( this(parent, child), logPseudoCountMap( child ) ) - maxes(parent)
              )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val defaultDenom = subtractLogProb(
      logSum( logPseudoCountMap.values.toSeq ),
      log( logPseudoCountMap.values.size )
    )
    setDefaultChildMap( logPseudoCountMap.mapValues{ alpha => alpha - defaultDenom } )
  }

  def posteriorModeNormalize( pseudoCount:Double = 1D, alphaUnk:Boolean = true ) {
    val logPseudoCount = log( pseudoCount )
    val maxes = Map(
      cpt.keySet.map( parent => {
          val childCount = cpt( parent ).values.size
          if( childCount > 0 )
            parent -> subtractLogProb(
              logSum(
                log( pseudoCount*( childCount + { if( alphaUnk ) 1D else 0D } ) )::cpt(parent).values.toList
              ),
              log( childCount )
            )
          else
            parent -> Double.NegativeInfinity
            //parent -> 0D
        }
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> ( subtractLogProb( logSum( this(parent, child), logPseudoCount ), 0D ) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    if( alphaUnk )
      setDefaultParentMap(
        Map(
          cpt.keySet.map{ parent =>
            parent -> { subtractLogProb( logPseudoCount, 0D ) - maxes( parent ) }
          }.toSeq:_*
        )
      )
  }

  def posteriorModeNormalize( pseudoCountMap:scala.collection.Map[U,Double] ) {
    val logPseudoCountMap = pseudoCountMap.mapValues( log( _ ) )
    val maxes = Map(
      cpt.keySet.map( parent => {
          val childCount = cpt( parent ).values.size
          if( childCount > 0 )
            parent -> subtractLogProb(
              logSum( log(pseudoCountMap.values.reduce(_+_))::cpt(parent).values.toList ),
              log( childCount )
            )
          else
            parent -> Double.NegativeInfinity
        }
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> (
                subtractLogProb(
                  logSum( this(parent, child), logPseudoCountMap( child ) ),
                  0D
                ) - maxes(parent)
              )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val defaultDenom = subtractLogProb(
      logSum( logPseudoCountMap.values.toSeq ),
      log( logPseudoCountMap.values.size )
    )
    setDefaultChildMap( logPseudoCountMap.mapValues{ alpha => subtractLogProb( alpha, 0D ) - defaultDenom } )
  }

  // right now assumes symmetric prior
  def expDigammaNormalize( pseudoCount:Double = 1D, alphaUnk:Boolean = true ) {
    val logPseudoCount = log( pseudoCount )
    val maxes = Map(
      cpt.keySet.map( parent =>
        if( cpt( parent ).values.size > 0 )
          parent -> expDigamma(
            // child count plus one to allow for unseen children
            logSum(log( pseudoCount * ( cpt(parent).values.size + { if( alphaUnk ) 1D else 0D } ) )::cpt(parent).values.toList)
          )
        else
          parent -> Double.NegativeInfinity
          //parent -> 0D
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> (
                expDigamma(
                  logSum(
                    this(parent, child),
                    logPseudoCount
                  )
                //) - expDigamma( maxes(parent) )
                ) - maxes(parent)
              )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    if( !  cpt.keySet.forall{ parent => logSum( cpt( parent ).values.toSeq ) <= 0D } ) {
      println( toString )
    }

    assert(
      cpt.keySet.forall{ parent =>
        logSum( cpt( parent ).values.toSeq ) <= 0D
      }
    )

    if( alphaUnk )
      setDefaultParentMap(
        Map(
          cpt.keySet.map{ parent =>
            parent -> { expDigamma( logPseudoCount ) - maxes( parent ) }
          }.toSeq:_*
        )/*.withDefaultValue( 
          expDigamma( logPseudoCount ) - expDigamma( math.log( parents.size) + logPseudoCount )
        )*/
      )
      // setDefault(
      //   expDigamma( logPseudoCount ) - expDigamma( math.log( parents.size +1 ) + logPseudoCount )
      // )
  }

  // right now assumes symmetric prior
  def nonRaggedExpDigammaNormalize( pseudoCount:Double = 1D ) {
    val logPseudoCount = log( pseudoCount )

    val allChildren = cpt.values.flatMap{ _.keySet }
    val childCount = allChildren.size

    val maxes = Map(
      cpt.keySet.map( parent =>
        if( cpt( parent ).values.size > 0 )
          parent -> expDigamma(
            //logSum(log( pseudoCount * cpt(parent).values.size )::cpt(parent).values.toList)
            logSum(log( pseudoCount * childCount )::cpt(parent).values.toList)
          )
        else
          parent -> Double.NegativeInfinity
          //parent -> 0D
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> (
                expDigamma(
                  logSum(
                    this(parent, child),
                    logPseudoCount
                  )
                ) - maxes(parent)
              )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    assert(
      cpt.keySet.forall{ parent =>
        logSum( cpt( parent ).values.toSeq ) <= 0D
      }
    )

    setDefaultParentMap(
      Map(
        cpt.keySet.map{ parent =>
          parent -> { expDigamma( logPseudoCount ) - maxes( parent ) }
        }.toSeq:_*
      ).withDefaultValue( 
        expDigamma( logPseudoCount ) - expDigamma( math.log( parents.size) + logPseudoCount )
      )
    )
  }

  def expDigammaNormalize( pseudoCountMap:scala.collection.Map[U,Double] ) {
    //val logPseudoCount = log( pseudoCount )
    val logPseudoCountMap = pseudoCountMap.mapValues( log( _ ) )
    val maxes = Map(
      cpt.keySet.map( parent =>
        if( cpt( parent ).values.size > 0 )
          parent -> expDigamma(
            logSum( log(pseudoCountMap.values.reduce(_+_))::cpt(parent).values.toList )
          )
        else
          parent -> Double.NegativeInfinity
          //parent -> 0D
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> (
                expDigamma(
                  logSum(
                    this(parent, child),
                    logPseudoCountMap( child )
                  )
                ) - maxes(parent)
              )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    cpt.keySet.foreach{ parent =>
      if( !( logSum( cpt( parent ).values.toSeq ) <= 0D ) ) {
        println(
          "} " + parent + ": " + logSum( cpt( parent ).values.toSeq ) +
          cpt(parent).keySet.map{ child => child + ": " + cpt(parent)(child) }.mkString("\n\t","\n\t","\n\n" )
        )
      }
    }

    assert(
      cpt.keySet.forall{ parent =>
        logSum( cpt( parent ).values.toSeq ) <= 0D
      }
    )

    val defaultDenom = expDigamma( logSum( logPseudoCountMap.values.toSeq ) )
    setDefaultChildMap( logPseudoCountMap.mapValues{ alpha => expDigamma( alpha ) - defaultDenom } )

      // logPseudoCountMap.mapValues{ alpha =>
      //   alpha - logSum( logPseudoCountMap.values.toSeq )
      // }
  }


      // def expDigammaNormalize( pseudoCountMap:scala.collection.Map[U,Double] ) {
      //   //val logPseudoCount = log( pseudoCount )
      //   val logPseudoCountMap = pseudoCountMap.mapValues( log( _ ) )

      //   val allChildren = cpt.values.flatMap{ _.keySet }
      //   val childCount = allChildren.size

      //   val maxes = Map(
      //     cpt.keySet.map( parent =>
      //       if( cpt( parent ).values.size > 0 )
      //         parent -> expDigamma(
      //           logSum( log(pseudoCountMap.values.reduce(_+_))::cpt(parent).values.toList )
      //         )
      //       else
      //         parent -> Double.NegativeInfinity
      //         //parent -> 0D
      //     ).toSeq:_*
      //   )

      //   cpt = Map(
      //     cpt.keySet.map{ parent =>
      //       parent -> Map(
      //         cpt(parent).keySet.map{ child =>
      //           if( maxes( parent ) == Double.NegativeInfinity )
      //             child -> Double.NegativeInfinity
      //           else
      //             child -> (
      //               expDigamma(
      //                 logSum(
      //                   this(parent, child),
      //                   logPseudoCountMap( child )
      //                 )
      //               ) - maxes(parent)
      //             )
      //         }.toSeq:_*
      //       )
      //     }.toSeq:_*
      //   )

      //   cpt.keySet.foreach{ parent =>
      //     if( !( logSum( cpt( parent ).values.toSeq ) <= 0D ) ) {
      //       println(
      //         "} " + parent + ": " + logSum( cpt( parent ).values.toSeq ) +
      //         cpt(parent).keySet.map{ child => child + ": " + cpt(parent)(child) }.mkString("\n\t","\n\t","\n\n" )
      //       )
      //     }
      //   }

      //   assert(
      //     cpt.keySet.forall{ parent =>
      //       logSum( cpt( parent ).values.toSeq ) <= 0D
      //     }
      //   )

      //   //val defaultDenom = logSum( logPseudoCountMap.values.toSeq )
      //   //setDefaultChildMap( logPseudoCountMap.mapValues{ alpha => alpha - defaultDenom } )

      //     // logPseudoCountMap.mapValues{ alpha =>
      //     //   alpha - logSum( logPseudoCountMap.values.toSeq )
      //     // }
      // }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    cpt = Map(
    cpt.keySet.toList.sortWith{_>_}.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.toList.sortWith{_>_}.map{ child =>
            child -> ( log( r.nextDouble + centeredOn ) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    normalize
  }

  def +( otherCPT: AbstractLog2dTable[T,U] ) = {
    val parentsUnion = otherCPT.parents.toSet.union( parents.toSet )
    //val childrenUnion = otherCPT.children.toSet.union( children.toSet )
    val summedCPT = Map(
      parentsUnion.map{ parent =>
        parent -> Map(
          ( this(parent).keySet ++ otherCPT(parent).keySet ).map{ child =>
            child ->
              //Math.sumLogProb( this( parent , child ), otherCPT( parent , child ) )
              logSum( this( parent , child ), otherCPT( parent , child ) )
          }.toSeq:_*
        )
      }.toSeq:_*
    )

    val toReturn = new Log2dTable( parentsUnion.toSet, Set[U]() ) //childrenUnion.toSet )
    toReturn.setCPTMap( summedCPT )


    toReturn
  }

  def getCPT = cpt

  def parents = cpt.keySet

  override def toString =
  "\tdefaultChildMap:" + defaultChildMap.keySet.map{ child =>
      child + ": " + /*math.exp(*/ defaultChildMap( child ) /*)*/ + "\n"
    }.mkString("\n\t\t","\n\t\t","\n") +
    parents.toList.sortWith( (a,b) => a < b ).map{ parent =>
    "\n\t" + parent + " default: " + /*math.exp(*/ getParentDefault( parent ) /*)*/ + 
    this(parent).keySet.toList.sortWith( (a,b) => a < b ).map{ ch =>
      //parent + " --> " +ch + ":\t" + ( "%1.4f".format( exp( cpt(parent)(ch) ) ) )
      parent + " --> " +ch + ":\t" + /*math.exp(*/ cpt(parent)(ch) /*)*/
    }.mkString("\n\t","\n\t","")
  }.mkString("","\n","\n")
}

/*
 * Basic properties for a conditional probability table (basically, initialize uniformly s.t. the
 * right things sum to 1)
 */
class LogCPT[T<:Label,U<:Label]( passedParents:Iterable[T], passedChildren:Iterable[U] )
  extends AbstractLog2dTable[T,U] {
  var cpt = Map(
    passedParents.map{ parent =>
        parent ->
          Map(
            passedChildren.map{ child =>
              child -> log( 1D/passedChildren.size )
            }.toSeq: _*
          )
      }.toSeq: _*
    )
}

/*
 * Basic properties for a 2-dimensional table (basically, initialize everything to 0 in log space)
 */
class Log2dTable[T<:Label,U<:Label]( passedParents:Iterable[T], passedChildren:Iterable[U] )
  extends AbstractLog2dTable[T,U] {

  def hallucinateCounts( hallucination:Map[T,Double] ) {
    cpt = Map(
      parents.map( parent =>
        parent ->
          Map(
            this(parent).keySet.map( child =>
              // child -> Math.sumLogProb(
              //   cpt(parent)(child),
              //   hallucination(parent)
              // )
              child -> logSum(
                cpt(parent)(child),
                hallucination(parent)
              )
            ).toSeq: _*
          )
      ).toSeq: _*
    )
  }

  var cpt = Map(
    passedParents.map( parent =>
        parent ->
          Map(
            passedChildren.map( child =>
              child -> Double.NegativeInfinity
            ).toSeq: _*
          )
      ).toSeq: _*
    )

  def divideBy( divisor: Double ) {
    cpt.keySet.foreach{ parent =>
      cpt(parent).keySet.foreach{ child =>
        cpt(parent)(child) = cpt(parent)(child) - divisor
      }
    }
  }

  def divideBy( divisorMap: collection.immutable.Map[T,Double] ) {
    cpt.keySet.foreach{ parent =>
      cpt(parent).keySet.foreach{ child =>
        cpt(parent)(child) = cpt(parent)(child) - divisorMap( parent )
      }
    }
  }

  def multiplyBy( multiplicand: Double ) {
    cpt.keySet.foreach{ parent =>
      cpt(parent).keySet.foreach{ child =>
        cpt(parent)(child) = cpt(parent)(child) + multiplicand
      }
    }
  }

  def toLogCPT = {
    val toReturn = new LogCPT( parents, Set[U]() )
    toReturn.setCPTMap( cpt )
    toReturn.normalize
    toReturn.setDefault( getDefault )
    toReturn.setDefaultParentMap( getDefaultParentMap )
    toReturn.setDefaultChildMap( getDefaultChildMap )
    toReturn
  }
  def asLogCPT = {
    val toReturn = new LogCPT( parents, Set[U]() )
    toReturn.setCPTMap( cpt )
    toReturn.setDefault( getDefault )
    toReturn.setDefaultParentMap( getDefaultParentMap )
    toReturn.setDefaultChildMap( getDefaultChildMap )
    toReturn
  }
}

object Log2dTable {
  def apply[T<:Label,U<:Label]( parents:Set[T], children:Set[U], defaultMap:collection.immutable.Map[U,Double] ) = {
    val toReturn = new Log2dTable( parents, children )
    toReturn.setDefaultChildMap( defaultMap )
    toReturn
  }
  def apply[T<:Label,U<:Label]( parents:Set[T], children:Set[U], defaultMap:collection.mutable.Map[U,Double] ) = {
    val toReturn = new Log2dTable( parents, children )
    toReturn.setDefaultChildMap( defaultMap )
    toReturn
  }
}

    // class TiedLog2dTable[T<:Label,U<:Label](
    //   passedParents:Iterable[T],
    //   passedChildren:Iterable[U],
    //   tyingRelation:((T,U) => T)
    // ) extends Log2dTable( passedParents, passedChildren ) {
    // 
    //   def getTiedElements( parent:T, child:U ) =
    //     cpt.keySet.filter( tyingRelation( _, child ) == tyingRelation( parent, child ) )
    // 
    //   def getTiedValues( parent:T, child:U ) =
    //     getTiedElements( parent, child ).map{ tiedParent => this( tiedParent, child ) }
    // 
    // 
    //   // This assumes that tied rules have been been divided by the number of tied rules, i.e. that we
    //   // ARE using tiedStopRuleCount in partialCounts.DMVFullBayesianBackoffPartialCounts
    //   override def expDigammaNormalize( pseudoCountMap:scala.collection.Map[U,Double] ) {
    // 
    //     val logPseudoCountMap = pseudoCountMap.mapValues( log( _ ) )
    // 
    //     val maxes = Map(
    //       cpt.keySet.map( parent =>
    //         if( cpt( parent ).values.size > 0 )
    //           parent -> expDigamma(
    //             logSum( log(pseudoCountMap.values.reduce(_+_))::{
    //                 cpt(parent).keySet.flatMap{ child => getTiedValues( parent, child ) }
    //                 //cpt(parent)
    //               }.toList //.values.toList
    //             )
    //           )
    //         else
    //           parent -> Double.NegativeInfinity
    //           //parent -> 0D
    //       ).toSeq:_*
    //     )
    // 
    //     cpt = Map(
    //       cpt.keySet.map{ parent =>
    //         parent -> Map(
    //           cpt(parent).keySet.map{ child =>
    //             if( maxes( parent ) == Double.NegativeInfinity )
    //               child -> Double.NegativeInfinity
    //             else
    //               child -> (
    //                 expDigamma(
    //                   logSum(
    //                     //this(parent, child),
    //                     logPseudoCountMap( child )::(getTiedValues( parent, child ).toList)
    //                   )
    //                 ) - maxes(parent)
    //               )
    //           }.toSeq:_*
    //         )
    //       }.toSeq:_*
    //     )
    // 
    //     cpt.keySet.foreach{ parent =>
    //       if( !( logSum( cpt( parent ).values.toSeq ) <= 0D ) ) {
    //         println(
    //           "} " + parent + ": " + logSum( cpt( parent ).values.toSeq ) +
    //           cpt(parent).keySet.map{ child => child + ": " + cpt(parent)(child) }.mkString("\n\t","\n\t","\n\n" )
    //         )
    //       }
    //     }
    // 
    //     assert(
    //       cpt.keySet.forall{ parent =>
    //         logSum( cpt( parent ).values.toSeq ) <= 0D
    //       }
    //     )
    // 
    //     val defaultDenom = logSum( logPseudoCountMap.values.toSeq )
    //     setDefaultChildMap( logPseudoCountMap.mapValues{ alpha => alpha - defaultDenom } )
    // 
    //       // logPseudoCountMap.mapValues{ alpha =>
    //       //   alpha - logSum( logPseudoCountMap.values.toSeq )
    //       // }
    //   }
    // 
    // }

/*
 * Basic properties for a 1-dimensional table 
 */
abstract class AbstractLog1dTable[T<:Label] extends AbstractTable {
  protected var pt:Map[T,Double]

  def setPTMap( updatedPT: Map[T,Double] ) {
    pt = updatedPT
  }
  def setPT( updatedPT: AbstractLog1dTable[T] ) {
    pt = updatedPT.getPT
    setDefault( updatedPT.getDefault )
  }

  def getPT = pt

  //def setValue( element:T, newValue:Double ) { pt = pt ++ Map( element -> newValue ) }
  def setValue( element:T, newValue:Double ) { pt += element -> newValue }

  def +( otherPT: AbstractLog1dTable[T] ) = {
    val domainUnion = otherPT.domain.union( domain )

    val summedPT = Map(
      domainUnion.map{ element =>
        // element -> Math.sumLogProb(
        //   this( element ),
        //   otherPT( element )
        // )
        element -> logSum(
          this( element ),
          otherPT( element )
        )
      }.toSeq:_*
    )

    val toReturn = new Log1dTable[T]( domainUnion.toSet ) 

    toReturn.setPTMap( summedPT )
    toReturn
  }

  def normalize {
    //val max = pt.values.reduceLeft( Math.sumLogProb( _ , _) )
    val max = logSum( pt.values.toSeq )

    pt = Map(
      pt.keySet.map{ parent =>
        if( max == Double.NegativeInfinity )
          parent -> Double.NegativeInfinity
        else
          parent -> ( pt(parent) - max )
      }.toSeq:_*
    )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    pt = Map(
      pt.keySet.map{ parent =>
        parent ->  ( log( r.nextDouble + centeredOn ) )
      }.toSeq:_*
    )

    normalize
  }

  def domain = pt.keySet

  def apply( k:T ) = pt.getOrElse( k, getDefault )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    //parent + ":\t" + ( "%1.4f".format( exp( pt(parent) ) ) )
    parent + ":\t" + math.exp( pt(parent) )
  }.mkString("\n\t","\n\t","\n")
}

/*
 * Basic properties for a 1-dimensional probability table (basically, initialize uniformly s.t.
 * everything sums to 1)
 */
class LogPT[T<:Label]( passedDomain:Iterable[T] ) extends AbstractLog1dTable[T] {
  var pt = Map(
    passedDomain.map( element =>
      element -> log( 1D/ passedDomain.size )
    ).toSeq: _*
  )
}

/*
 * Basic properties for a 1-dimensional table (basically initialize everything to 0 in log space)
 */
class Log1dTable[T<:Label]( passedDomain:Iterable[T] ) extends AbstractLog1dTable[T] {
  var pt = Map(
    passedDomain.map( element =>
      element -> Double.NegativeInfinity
    ).toSeq: _*
  )

  def toLogPT = {
    val toReturn = new LogPT( domain )
    toReturn.setPTMap( pt )
    toReturn.normalize
    toReturn
  }
}

object Log1dTable {
  def apply[T<:Label]( domain:Set[T], defaultVal:Double ) = {
    val toReturn = new Log1dTable( domain )
    toReturn.setDefault( defaultVal )
    toReturn
  }
}

