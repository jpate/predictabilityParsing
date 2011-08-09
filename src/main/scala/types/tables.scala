package predictabilityParsing.types.tables

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
}

/*
 * Basic properties for a 2-dimensional table (with rows and columns)
 */
abstract class AbstractLog2dTable[T<:Label,U<:Label]
  extends AbstractTable {

  var cpt:Map[T,Map[U,Double]]

  def apply( k:T ) = cpt( k )

  def setCPT( updatedCPT: Map[T,Map[U,Double]] ) {
    cpt = updatedCPT
  }

  def normalize {
    val maxes = Map(
      cpt.keySet.map( parent =>
        parent -> ( cpt(parent).values/*.par*/.reduceLeft( Math.sumLogProb(_,_) ) )
      ).toSeq:_*
    )

    cpt = Map(
      cpt.keySet.map{ parent =>
        parent -> Map(
          cpt(parent).keySet.map{ child =>
            if( maxes( parent ) == Double.NegativeInfinity )
              child -> Double.NegativeInfinity
            else
              child -> ( cpt(parent)(child) - maxes(parent) )
          }.toSeq:_*
        )
      }.toSeq:_*
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
    val childrenUnion = otherCPT.children.toSet.union( children.toSet )
    val summedCPT = Map(
      parentsUnion.map{ parent =>
        parent -> Map(
          childrenUnion.map{ child =>
            child -> Math.sumLogProb(
              cpt( parent )( child ),
              otherCPT( parent )( child )
            )
          }.toSeq:_*
        ).withDefaultValue( Double.NegativeInfinity )
      }.toSeq:_*
    ).withDefaultValue( Map().withDefaultValue( Double.NegativeInfinity ) )

    val toReturn = new Log2dTable( parentsUnion.toSet, childrenUnion.toSet )
    toReturn.setCPT( summedCPT )


    toReturn
  }

  def parents = cpt.keySet
  def children = cpt.values.head.keySet

  override def toString = parents.toList.sortWith( (a,b) => a < b ).map{ parent =>
    children.toList.sortWith( (a,b) => a < b ).map{ ch =>
      parent + " --> " +ch + ":\t" + exp( cpt(parent)(ch) )
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
          ).withDefaultValue( Double.NegativeInfinity )
      }.toSeq: _*
    ).withDefaultValue( Map().withDefaultValue( Double.NegativeInfinity ) )
}

/*
 * Basic properties for a 2-dimensional table (basically, initialize everything to 0 in log space)
 */
class Log2dTable[T<:Label,U<:Label]( passedParents:Iterable[T], passedChildren:Iterable[U] )
  extends AbstractLog2dTable[T,U] {

  var cpt = Map(
    passedParents.map( parent =>
        parent ->
          Map(
            passedChildren.map( child =>
              child -> Double.NegativeInfinity
            ).toSeq: _*
          ).withDefaultValue( Double.NegativeInfinity )
      ).toSeq: _*
    ).withDefaultValue( Map().withDefaultValue( Double.NegativeInfinity ) )

  def toLogCPT = {
    val toReturn = new LogCPT( parents, children )
    toReturn.setCPT( cpt )
    //toReturn.normalize
    toReturn
  }
}

/*
 * Basic properties for a 1-dimensional table 
 */
abstract class AbstractLog1dTable[T<:Label] extends AbstractTable {
  var pt:Map[T,Double]

  def setPT( updatedPT: Map[T,Double] ) {
    pt = updatedPT
  }

  def +( otherPT: AbstractLog1dTable[T] ) = {
    val domainUnion = otherPT.domain.union( domain )

    val summedPT = Map(
      domainUnion.map{ element =>
        element -> Math.sumLogProb(
          pt( element ),
          otherPT( element )
        )
      }.toSeq:_*
    ).withDefaultValue( Double.NegativeInfinity )

    val toReturn = new Log1dTable[T]( domainUnion.toSet ) 

    toReturn.setPT( summedPT )
    toReturn
  }

  def normalize {
    val max = pt.values.reduceLeft( Math.sumLogProb( _ , _) )

    pt = Map(
      pt.keySet.map{ parent =>
          // by convention 0 / 0 = 0
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

  def apply( k:T ) = pt( k )

  override def toString = pt.keySet.toList.sortWith( (a,b) => a < b ).map{ parent =>
    parent + ":\t" + exp( pt(parent) )
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
    toReturn.setPT( pt )
    toReturn.normalize
    toReturn
  }
}

