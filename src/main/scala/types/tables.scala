package predictabilityParsing.types.tables

import scalala.library.Numerics.logSum
import predictabilityParsing.types.labels._
import predictabilityParsing.util.Math
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
 */
abstract class AbstractLog2dTable[T<:Label,U<:Label]
  extends AbstractTable {

  protected var cpt:Map[T,Map[U,Double]]


  def values = cpt.values
  def apply( k:T ) = cpt.getOrElse( k, Map[U,Double]().withDefaultValue( getDefault ) )
  def apply( parent:T, child:U ) =
    cpt.getOrElse(
      parent,
      Map( child -> super.getDefault )
    ).getOrElse(
      child,
      defaultMap( parent )
    )

  def getDefault( k:T ):Double = defaultMap( k )

  private val defaultMap = Map[T,Double]().withDefaultValue( super.getDefault )
  def setDefaultMap( newDefaultMap:collection.mutable.Map[T,Double] ) {
    defaultMap.clear
    defaultMap ++= newDefaultMap
  }
  def setDefaultMap( newDefaultMap:collection.immutable.Map[T,Double] ) {
    defaultMap.clear
    defaultMap ++= newDefaultMap
  }

  def getDefaultMap = defaultMap

  def setCPTMap( updatedCPT: Map[T,Map[U,Double]] ) {
    cpt = updatedCPT
  }

  def setCPT( updatedCPT: AbstractLog2dTable[T,U] ) {
    cpt = updatedCPT.getCPT
    setDefault( updatedCPT.getDefault )
    setDefaultMap( updatedCPT.getDefaultMap )
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

  // expDigamma without exp because we are in log-space
  private def expDigamma( input:Double ) = 
      {
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

  // right now assumes symmetric prior
  def expDigammaNormalize( pseudoCount:Double = 1D ) {
    val logPseudoCount = log( pseudoCount )
    val maxes = Map(
      cpt.keySet.map( parent =>
        if( cpt( parent ).values.size > 0 )
          parent -> expDigamma(
            //((log( pseudoCount * cpt(parent).values.size ))::cpt(parent).values.toList).reduce( Math.sumLogProb(_,_) )
            logSum((log( pseudoCount * cpt(parent).values.size ))::cpt(parent).values.toList)
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
                ) - expDigamma( maxes(parent) )
              )
          }.toSeq:_*
        )//.withDefaultValue( expDigamma( logPseudoCount ) - maxes(parent) )
      }.toSeq:_*
    )/*.withDefaultValue(
      Map[U,Double]().withDefaultValue(
        expDigamma( logPseudoCount ) -
          expDigamma( math.log( parents.size ) + logPseudoCount )
      )
    )*/

    setDefaultMap(
      Map(
        cpt.keySet.map{ parent =>
          parent -> { expDigamma( logPseudoCount ) - expDigamma( maxes( parent ) ) }
        }.toSeq:_*
      )
    )
    setDefault(
      expDigamma( logPseudoCount ) - expDigamma( math.log( parents.size ) + logPseudoCount )
    )
  }

  def randomize( seed:Int, centeredOn:Int ) {
    import scala.util.Random
    val r = new Random( seed )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
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

  override def toString = parents.toList.sortWith( (a,b) => a < b ).map{ parent =>
    this(parent).keySet.toList.sortWith( (a,b) => a < b ).map{ ch =>
      //parent + " --> " +ch + ":\t" + ( "%1.4f".format( exp( cpt(parent)(ch) ) ) )
      parent + " --> " +ch + ":\t" + math.exp( cpt(parent)(ch) )
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
    toReturn.setDefaultMap( getDefaultMap )
    toReturn
  }
  def asLogCPT = {
    val toReturn = new LogCPT( parents, Set[U]() )
    toReturn.setCPTMap( cpt )
    toReturn.setDefault( getDefault )
    toReturn.setDefaultMap( getDefaultMap )
    toReturn
  }
}

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

