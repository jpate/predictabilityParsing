    // package predictabilityParsing.partialCounts
    // 
    // import scalala.library.Numerics.{lgamma,logSum}
    // import predictabilityParsing.types.labels._
    // import predictabilityParsing.types.tables._
    // import predictabilityParsing.grammars.DMVBayesianBackoffSimpleThreeStreamGrammar
    // import predictabilityParsing.grammars.AbstractDMVGrammar
    // import predictabilityParsing.util.Math
    // 
    // /*
    //  *
    //  *  This is similar to DMVBayesianBackoffPartialCounts, except it manages backoff among THREE
    //  *  streams for both head and dependant. The space of possible components in this mixture is large,
    //  *  but I'm going to assume that it is not necessary to represent every possible conditioning
    //  *  structure. Instead, we will condition in the following components for streams w (words), p
    //  *  (POS), and a (acoustics):
    //  *
    //  *  ( h_w, h_p, h_a, d_w, d_p, d_a )          no backoff
    //  *  ( h_p, h_a, d_p, d_a )                    backoff words
    //  *  ( h_w, h_p, d_w, d_p )                    backoff acoustics
    //  *  ( h_p, d_p )                              backoff words and acoustics
    //  *  ( h_w, d_w )                              backoff POS and acoustics
    //  *
    //  *  We will always use lexical identity for selecting sentence root.
    //  *
    //  *  Finally, I apologize for the absolutely monstrous number of parameters in the class
    //  *  constructor...
    //  *
    //  */
    // class DMVBayesianBackoffSimpleThreeStreamPartialCounts(
    //   // These are hyperparameters (i.e. alphas) for the dirichlets from which choose and stop backoff
    //   // decisions are drawn. Right now, let's make choose and stop priors use the same hyperparameters.
    //   noBackoff_Alpha:Double = 35,
    //   backoffW_Alpha:Double = 35,
    //   backoffA_Alpha:Double = 35,
    //   backoffWA_Alpha:Double = 35,
    //   backoffPA_Alpha:Double = 35,
    //   // these are specific backoff parameters
    //   noStopBackoff_Score:AbstractLog1dTable[StopOrNot],
    //   stopBackoffW_Score:AbstractLog1dTable[StopOrNot],
    //   stopBackoffA_Score:AbstractLog1dTable[StopOrNot],
    //   stopBackoffWA_Score:AbstractLog1dTable[StopOrNot],
    //   stopBackoffPA_Score:AbstractLog1dTable[StopOrNot],
    //   noChooseBackoff_Score:AbstractLog1dTable[ChooseArgument],
    //   chooseBackoffW_Score:AbstractLog1dTable[ChooseArgument],
    //   chooseBackoffA_Score:AbstractLog1dTable[ChooseArgument],
    //   chooseBackoffWA_Score:AbstractLog1dTable[ChooseArgument],
    //   chooseBackoffPA_Score:AbstractLog1dTable[ChooseArgument]
    // ) extends DMVPartialCounts {
    // 
    // 
    //   def this(
    //     noBackoff_Alpha:Double,
    //     backoffW_Alpha:Double,
    //     backoffA_Alpha:Double,
    //     backoffWA_Alpha:Double,
    //     backoffPA_Alpha:Double
    //   ) = this(
    //     noBackoff_Alpha,
    //     backoffW_Alpha,
    //     backoffA_Alpha,
    //     backoffWA_Alpha,
    //     backoffPA_Alpha,
    //     // these are specific backoff parameters
    //     noStopBackoff_Score = Log1dTable(
    //       Set[StopOrNot](),
    //       Math.expDigamma( math.log( noBackoff_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     stopBackoffW_Score = Log1dTable(
    //       Set[StopOrNot](),
    //       Math.expDigamma( math.log( backoffW_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     stopBackoffA_Score = Log1dTable(
    //       Set[StopOrNot](),
    //       Math.expDigamma( math.log( backoffA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     stopBackoffWA_Score = Log1dTable(
    //       Set[StopOrNot](),
    //       Math.expDigamma( math.log( backoffWA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     stopBackoffPA_Score = Log1dTable(
    //       Set[StopOrNot](),
    //       Math.expDigamma( math.log( backoffPA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     noChooseBackoff_Score = Log1dTable(
    //       Set[ChooseArgument](),
    //       Math.expDigamma( math.log( noBackoff_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     chooseBackoffW_Score = Log1dTable(
    //       Set[ChooseArgument](),
    //       Math.expDigamma( math.log( backoffW_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     chooseBackoffA_Score = Log1dTable(
    //       Set[ChooseArgument](),
    //       Math.expDigamma( math.log( backoffA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     chooseBackoffWA_Score = Log1dTable(
    //       Set[ChooseArgument](),
    //       Math.expDigamma( math.log( backoffWA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     ),
    //     chooseBackoffPA_Score = Log1dTable(
    //       Set[ChooseArgument](),
    //       Math.expDigamma( math.log( backoffPA_Alpha ) ) -
    //         Math.expDigamma( math.log(
    //             noBackoff_Alpha +
    //             backoffW_Alpha +
    //             backoffA_Alpha +
    //             backoffWA_Alpha +
    //             backoffPA_Alpha
    //         ) )
    //     )
    //   )
    // 
    //   // override def clearInterpolationScores {
    //   //   println( "clearing interpolation scores..." )
    //   //   noStopBackoffScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( noStopBackoff ) ) -
    //   //         Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    //   //     )//.getPT
    //   //   )
    //   //   stopBackoffScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( stopBackoff ) ) -
    //   //         Math.expDigamma( math.log(noStopBackoff + stopBackoff) )
    //   //     )//.getPT
    //   //   )
    //   //   noChooseBackoffScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( noChooseBackoff ) ) -
    //   //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    //   //     )//.getPT
    //   //   )
    //   //   backoffHeadScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( backoffArg ) ) -
    //   //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    //   //     )//.getPT
    //   //   )
    //   //   backoffArgScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( backoffArg ) ) -
    //   //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    //   //     ) //.getPT
    //   //   )
    //   //   backoffBothScore.setPT(
    //   //     Log1dTable(
    //   //       Set[ObservedLabel](),
    //   //       Math.expDigamma( math.log( backoffBoth ) ) -
    //   //         Math.expDigamma( math.log(noChooseBackoff + backoffHead + backoffArg + backoffBoth ) )
    //   //     ) //.getPT
    //   //   )
    //   // }
    // 
    //   override def associatedGrammar = new DMVBayesianBackoffSimpleThreeStreamGrammar(
    //     // alphas
    //     noBackoff_Alpha,
    //     backoffW_Alpha,
    //     backoffA_Alpha,
    //     backoffWA_Alpha,
    //     backoffPA_Alpha,
    //     // interpolation params
    //     noStopBackoff_Score,
    //     stopBackoffW_Score,
    //     stopBackoffA_Score,
    //     stopBackoffWA_Score,
    //     stopBackoffPA_Score,
    //     noChooseBackoff_Score,
    //     chooseBackoffW_Score,
    //     chooseBackoffA_Score,
    //     chooseBackoffWA_Score,
    //     chooseBackoffPA_Score
    //   )
    // 
    // 
    //   override def toDMVGrammar = {
    //     print( "Computing DMVBayesianBackoffGrammar..." )
    // 
    // 
    // 
    //     // First, compute new interpolation parameters, starting with stop
    //     // val stopNoBackoffInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    //     // val stopBackoffWInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    //     // val stopBackoffAInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    //     // val stopBackoffWAInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    //     // val stopBackoffPAInterpolationSums = new Log1dTable( Set[ObservedLabel]() )
    //     val stopNoBackoffInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    //     val stopBackoffWInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    //     val stopBackoffAInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    //     val stopBackoffWAInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    //     val stopBackoffPAInterpolationSums = new Log1dTable( Set[StopOrNot]() )
    // 
    //     //val stopBackoffInterpolationDenom = new Log1dTable( Set[ObservedLabel]() )
    //     val stopBackoffInterpolationDenom = new Log1dTable( Set[StopOrNot]() )
    // 
    //     // // val stopBackoffWInterpolationTypeCounts = new Log1dTable( Set[ObservedLabel]() )
    //     // // val stopBackoffAInterpolationTypeCounts = new Log1dTable( Set[ObservedLabel]() )
    //     // // val stopBackoffWAInterpolationTypeCounts = new Log1dTable( Set[ObservedLabel]() )
    //     // // val stopBackoffPAInterpolationTypeCounts = new Log1dTable( Set[ObservedLabel]() )
    //     // val stopBackoffWInterpolationTypeCounts = new Log1dTable( Set[StopOrNot]() )
    //     // val stopBackoffAInterpolationTypeCounts = new Log1dTable( Set[StopOrNot]() )
    //     // val stopBackoffWAInterpolationTypeCounts = new Log1dTable( Set[StopOrNot]() )
    //     // val stopBackoffPAInterpolationTypeCounts = new Log1dTable( Set[StopOrNot]() )
    // 
    //     // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    //     val stopNoBackoffCounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     val stopBackoffWCounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     val stopBackoffACounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     val stopBackoffWACounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     val stopBackoffPACounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    // 
    // 
    //     // val stopBackoffWSumsTypeCounts =
    //     //   new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     // val stopBackoffASumsTypeCounts =
    //     //   new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     // val stopBackoffWASumsTypeCounts =
    //     //   new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     // val stopBackoffPASumsTypeCounts =
    //     //   new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    // 
    //     //println( "Summing over stopCounts to get counts for interpolation parameters and each backoff distribution." )
    //     stopCounts.parents.foreach{ stopKey =>
    //       stopKey.w match {
    //         case WordTriple( hw, hp, ha ) => {
    //           dmv.stopDecision.foreach{ dec =>
    //             val backoffStopW = WordPair( hp, ha )
    //             val backoffStopA = WordPair( hw, hp )
    //             val backoffStopWA = Word( hp )
    //             val backoffStopPA = Word( hw )
    // 
    //             val backoffStopWKey = StopOrNot( backoffStopW , stopKey.dir, stopKey.adj )
    //             val backoffStopAKey = StopOrNot( backoffStopA , stopKey.dir, stopKey.adj )
    //             val backoffStopWAKey = StopOrNot( backoffStopWA , stopKey.dir, stopKey.adj )
    //             val backoffStopPAKey = StopOrNot( backoffStopPA , stopKey.dir, stopKey.adj )
    // 
    //             val thisNoBackoffComponent =
    //               stopCounts( stopKey, dec ) + noStopBackoff_Score( stopKey /*stopKey.w*/ )
    //             val thisBackoffWComponent =
    //               stopCounts( stopKey, dec ) + stopBackoffW_Score( stopKey /*stopKey.w*/ )
    //             val thisBackoffAComponent =
    //               stopCounts( stopKey, dec ) + stopBackoffA_Score( stopKey /*stopKey.w*/ )
    //             val thisBackoffWAComponent =
    //               stopCounts( stopKey, dec ) + stopBackoffWA_Score( stopKey /*stopKey.w*/ )
    //             val thisBackoffPAComponent =
    //               stopCounts( stopKey, dec ) + stopBackoffPA_Score( stopKey /*stopKey.w*/ )
    // 
    //             // for interpolation parameters, sum out backoff set
    //             stopNoBackoffInterpolationSums.setValue(
    //               //stopKey.w,
    //               stopKey,
    //               logSum(
    //                 stopNoBackoffInterpolationSums( stopKey /*stopKey.w*/ ),
    //                 thisNoBackoffComponent
    //               )
    //             )
    //             // Use backoff heads because our interpolation rules are tied
    //             stopBackoffWInterpolationSums.setValue(
    //               //backoffStopW,
    //               backoffStopWKey,
    //               logSum(
    //                 stopBackoffWInterpolationSums( backoffStopWKey /*backoffStopW*/ ),
    //                 thisBackoffWComponent
    //               )
    //             )
    //             stopBackoffAInterpolationSums.setValue(
    //               //backoffStopA,
    //               backoffStopAKey,
    //               logSum(
    //                 stopBackoffAInterpolationSums( backoffStopAKey /*backoffStopA*/ ),
    //                 thisBackoffAComponent
    //               )
    //             )
    //             stopBackoffWAInterpolationSums.setValue(
    //               //backoffStopWA,
    //               backoffStopWAKey,
    //               logSum(
    //                 stopBackoffWAInterpolationSums( backoffStopWAKey /*backoffStopWA*/ ),
    //                 thisBackoffWAComponent
    //               )
    //             )
    //             stopBackoffPAInterpolationSums.setValue(
    //               //backoffStopPA,
    //               backoffStopPAKey,
    //               logSum(
    //                 stopBackoffPAInterpolationSums( backoffStopPAKey ),
    //                 thisBackoffPAComponent
    //               )
    //             )
    // 
    // 
    // 
    //             // for bringing tied counts down to average...
    //             // stopBackoffWInterpolationTypeCounts.setValue(
    //             //   //backoffStopW,
    //             //   backoffStopWKey,
    //             //   logSum(
    //             //     stopBackoffWInterpolationTypeCounts( backoffStopWKey /*backoffStopW*/ ), 0D
    //             //   )
    //             // )
    //             // stopBackoffAInterpolationTypeCounts.setValue(
    //             //   //backoffStopA,
    //             //   backoffStopAKey,
    //             //   logSum(
    //             //     stopBackoffAInterpolationTypeCounts( backoffStopAKey /*backoffStopA*/ ), 0D
    //             //   )
    //             // )
    //             // stopBackoffWAInterpolationTypeCounts.setValue(
    //             //   //backoffStopWA,
    //             //   backoffStopWAKey,
    //             //   logSum(
    //             //     stopBackoffWAInterpolationTypeCounts( backoffStopWAKey /*backoffStopWA*/ ), 0D
    //             //   )
    //             // )
    //             // stopBackoffPAInterpolationTypeCounts.setValue(
    //             //   //backoffStopPA,
    //             //   backoffStopPAKey,
    //             //   logSum(
    //             //     stopBackoffPAInterpolationTypeCounts( backoffStopPAKey /*backoffStopPA*/ ), 0D
    //             //   )
    //             // )
    // 
    // 
    // 
    //             // stopBackoffInterpolationDenom.setValue(
    //             //   //stopKey.w,
    //             //   stopKey,
    //             //   logSum(
    //             //     stopBackoffInterpolationDenom( stopKey /*stopKey.w*/ ),
    //             //     thisNoBackoffComponent
    //             //   )
    //             // )
    // 
    // 
    // 
    // 
    //             // for backoff distribution, only sum out elements of the backoff set.
    //             stopNoBackoffCounts.setValue(
    //               stopKey,
    //               dec,
    //               logSum(
    //                 stopNoBackoffCounts( stopKey, dec ),
    //                 thisNoBackoffComponent
    //               )
    //             )
    // 
    //             stopBackoffWCounts.setValue(
    //               backoffStopWKey,
    //               dec,
    //               logSum(
    //                 stopBackoffWCounts( backoffStopWKey, dec ),
    //                 thisBackoffWComponent
    //               )
    //             )
    //             stopBackoffACounts.setValue(
    //               backoffStopAKey,
    //               dec,
    //               logSum(
    //                 stopBackoffACounts( backoffStopAKey, dec ),
    //                 thisBackoffAComponent
    //               )
    //             )
    //             stopBackoffWACounts.setValue(
    //               backoffStopWAKey,
    //               dec,
    //               logSum(
    //                 stopBackoffWACounts( backoffStopWAKey, dec ),
    //                 thisBackoffWAComponent
    //               )
    //             )
    //             stopBackoffPACounts.setValue(
    //               backoffStopPAKey,
    //               dec,
    //               logSum(
    //                 stopBackoffPACounts( backoffStopPAKey, dec ),
    //                 thisBackoffPAComponent
    //               )
    //             )
    // 
    // 
    // 
    //             // for bringing tied counts down to average...
    //             // stopBackoffWSumsTypeCounts.setValue(
    //             //   backoffStopWKey,
    //             //   dec,
    //             //   logSum(
    //             //     stopBackoffWSumsTypeCounts( backoffStopWKey, dec ), 0D
    //             //   )
    //             // )
    //             // stopBackoffASumsTypeCounts.setValue(
    //             //   backoffStopAKey,
    //             //   dec,
    //             //   logSum(
    //             //     stopBackoffASumsTypeCounts( backoffStopAKey, dec ), 0D
    //             //   )
    //             // )
    //             // stopBackoffWASumsTypeCounts.setValue(
    //             //   backoffStopWAKey,
    //             //   dec,
    //             //   logSum(
    //             //     stopBackoffWASumsTypeCounts( backoffStopWAKey, dec ), 0D
    //             //   )
    //             // )
    //             // stopBackoffPASumsTypeCounts.setValue(
    //             //   backoffStopPAKey,
    //             //   dec,
    //             //   logSum(
    //             //     stopBackoffPASumsTypeCounts( backoffStopPAKey, dec ), 0D
    //             //   )
    //             // )
    // 
    // 
    // 
    //           }
    //         }
    //         case rootHead:AbstractRoot => { /* intentionally empty */ }
    //       }
    //     }
    // 
    //     val rootStopCounts =
    //       new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    // 
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, LeftAttachment, true ),
    //       Stop,
    //       Double.NegativeInfinity
    //     )
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, LeftAttachment, true ),
    //       NotStop,
    //       0D
    //     )
    // 
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, LeftAttachment, false ),
    //       Stop,
    //       0D
    //     )
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, LeftAttachment, false ),
    //       NotStop,
    //       Double.NegativeInfinity
    //     )
    // 
    // 
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, RightAttachment, true ),
    //       Stop,
    //       0D
    //     )
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, RightAttachment, true ),
    //       NotStop,
    //       Double.NegativeInfinity
    //     )
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, RightAttachment, false ),
    //       Stop,
    //       0D
    //     )
    //     rootStopCounts.setValue(
    //       StopOrNot( Root, RightAttachment, false ),
    //       NotStop,
    //       Double.NegativeInfinity
    //     )
    // 
    // 
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, RightAttachment, true ),
    //     //   Stop,
    //     //   0D
    //     // )
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, RightAttachment, true ),
    //     //   NotStop,
    //     //   Double.NegativeInfinity
    //     // )
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, RightAttachment, false ),
    //     //   Stop,
    //     //   0D
    //     // )
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, RightAttachment, false ),
    //     //   NotStop,
    //     //   Double.NegativeInfinity
    //     // )
    // 
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, LeftAttachment, true ),
    //     //   NotStop,
    //     //   0D
    //     // )
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, LeftAttachment, true ),
    //     //   Stop,
    //     //   Double.NegativeInfinity
    //     // )
    // 
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, LeftAttachment, false ),
    //     //   NotStop,
    //     //   Double.NegativeInfinity
    //     // )
    //     // rootStopCounts.setValue(
    //     //   StopOrNot( Root, LeftAttachment, false ),
    //     //   Stop,
    //     //   0D
    //     // )
    // 
    //     /*
    //     // bring tied rules to average:
    //           // stopBackoffWInterpolationTypeCounts.domain.foreach{ backoffWHead =>
    //           //   stopBackoffWInterpolationSums.setValue(
    //           //     backoffWHead,
    //           //     stopBackoffWInterpolationSums( backoffWHead ) -
    //           //       stopBackoffWInterpolationTypeCounts( backoffWHead )
    //           //   )
    //           // }
    //           // stopBackoffAInterpolationTypeCounts.domain.foreach{ backoffAHead =>
    //           //   stopBackoffAInterpolationSums.setValue(
    //           //     backoffAHead,
    //           //     stopBackoffAInterpolationSums( backoffAHead ) -
    //           //       stopBackoffAInterpolationTypeCounts( backoffAHead )
    //           //   )
    //           // }
    //           // stopBackoffWAInterpolationTypeCounts.domain.foreach{ backoffWAHead =>
    //           //   stopBackoffWAInterpolationSums.setValue(
    //           //     backoffWAHead,
    //           //     stopBackoffWAInterpolationSums( backoffWAHead ) -
    //           //       stopBackoffWAInterpolationTypeCounts( backoffWAHead )
    //           //   )
    //           // }
    //           // stopBackoffPAInterpolationTypeCounts.domain.foreach{ backoffPAHead =>
    //           //   stopBackoffPAInterpolationSums.setValue(
    //           //     backoffPAHead,
    //           //     stopBackoffPAInterpolationSums( backoffPAHead ) -
    //           //       stopBackoffPAInterpolationTypeCounts( backoffPAHead )
    //           //   )
    //           // }
    // 
    //     stopBackoffWCounts.parents.foreach{ backoffWHead =>
    //       dmv.stopDecision.foreach{ dec =>
    //         stopBackoffWCounts.setValue(
    //           backoffWHead,
    //           dec,
    //           stopBackoffWCounts( backoffWHead, dec ) -
    //             stopBackoffWSumsTypeCounts( backoffWHead, dec )
    //         )
    //       }
    //     }
    //     stopBackoffACounts.parents.foreach{ backoffAHead =>
    //       dmv.stopDecision.foreach{ dec =>
    //         stopBackoffACounts.setValue(
    //           backoffAHead,
    //           dec,
    //           stopBackoffACounts( backoffAHead, dec ) -
    //             stopBackoffASumsTypeCounts( backoffAHead, dec )
    //         )
    //       }
    //     }
    //     stopBackoffWACounts.parents.foreach{ backoffWAHead =>
    //       dmv.stopDecision.foreach{ dec =>
    //         stopBackoffWACounts.setValue(
    //           backoffWAHead,
    //           dec,
    //           stopBackoffWACounts( backoffWAHead, dec ) -
    //             stopBackoffWASumsTypeCounts( backoffWAHead, dec )
    //         )
    //       }
    //     }
    //     stopBackoffPACounts.parents.foreach{ backoffPAHead =>
    //       dmv.stopDecision.foreach{ dec =>
    //         stopBackoffPACounts.setValue(
    //           backoffPAHead,
    //           dec,
    //           stopBackoffPACounts( backoffPAHead, dec ) -
    //             stopBackoffPASumsTypeCounts( backoffPAHead, dec )
    //         )
    //       }
    //     }
    //     */
    // 
    // 
    //     //println( "Finishing up sums for stop denominator." )
    //     //stopBackoffInterpolationDenom.domain.foreach{ denom =>
    //     stopNoBackoffInterpolationSums.domain.foreach{ denom =>
    //       val WordTriple( hw, hp, ha ) = denom.w
    //       val backoffStopW = WordPair( hp, ha )
    //       val backoffStopA = WordPair( hw, hp )
    //       val backoffStopWA = Word( hp )
    //       val backoffStopPA = Word( hw )
    // 
    //       val backoffStopWKey = StopOrNot( backoffStopW , denom.dir, denom.adj )
    //       val backoffStopAKey = StopOrNot( backoffStopA , denom.dir, denom.adj )
    //       val backoffStopWAKey = StopOrNot( backoffStopWA , denom.dir, denom.adj )
    //       val backoffStopPAKey = StopOrNot( backoffStopPA , denom.dir, denom.adj )
    // 
    // 
    //       stopBackoffInterpolationDenom.setValue(
    //         denom,
    //         logSum(
    //           Seq(
    //             //stopBackoffInterpolationDenom( denom ),
    //             stopNoBackoffInterpolationSums( denom ),
    //             stopBackoffWInterpolationSums( backoffStopWKey ),
    //             stopBackoffAInterpolationSums( backoffStopAKey ),
    //             stopBackoffWAInterpolationSums( backoffStopWAKey ),
    //             stopBackoffPAInterpolationSums( backoffStopPAKey ),
    //             math.log(
    //               noBackoff_Alpha +
    //               backoffW_Alpha +
    //               backoffA_Alpha +
    //               backoffWA_Alpha +
    //               backoffPA_Alpha
    //             )
    //           )
    //         )
    //       )
    // 
    //     }
    // 
    //     val newNoStopBackoff_Score = new Log1dTable( Set[StopOrNot]() )
    //     val newStopBackoffW_Score = new Log1dTable( Set[StopOrNot]() )
    //     val newStopBackoffA_Score = new Log1dTable( Set[StopOrNot]() )
    //     val newStopBackoffWA_Score = new Log1dTable( Set[StopOrNot]() )
    //     val newStopBackoffPA_Score = new Log1dTable( Set[StopOrNot]() )
    // 
    // 
    //     //println( "Estimating new stop interpolation distributions." )
    //     stopBackoffInterpolationDenom.domain.foreach{ h =>
    // 
    //       val WordTriple( hw, hp, ha ) = h.w
    // 
    //       val backoffStopW = WordPair( hp, ha )
    //       val backoffStopA = WordPair( hw, hp )
    //       val backoffStopWA = Word( hp )
    //       val backoffStopPA = Word( hw )
    // 
    //       val backoffStopWKey = StopOrNot( backoffStopW , h.dir, h.adj )
    //       val backoffStopAKey = StopOrNot( backoffStopA , h.dir, h.adj )
    //       val backoffStopWAKey = StopOrNot( backoffStopWA , h.dir, h.adj )
    //       val backoffStopPAKey = StopOrNot( backoffStopPA , h.dir, h.adj )
    // 
    // 
    //       val expDigammaDenom = Math.expDigamma( stopBackoffInterpolationDenom( h ) )
    // 
    //       val thisStopNoBackoffTotal = logSum(
    //         stopNoBackoffInterpolationSums( h ),
    //         math.log( noBackoff_Alpha )
    //       )
    //       newNoStopBackoff_Score.setValue(
    //         h,
    //         Math.expDigamma( thisStopNoBackoffTotal) - expDigammaDenom
    //       )
    // 
    // 
    //       val thisStopBackoffWTotal = logSum(
    //         stopBackoffWInterpolationSums( backoffStopWKey /*backoffStopW*/ ),
    //         math.log( backoffW_Alpha )
    //       )
    //       newStopBackoffW_Score.setValue(
    //         h,
    //         Math.expDigamma(
    //           thisStopBackoffWTotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisStopBackoffATotal = logSum(
    //         stopBackoffAInterpolationSums( backoffStopAKey /*backoffStopA*/ ),
    //         math.log( backoffA_Alpha )
    //       )
    //       newStopBackoffA_Score.setValue(
    //         h,
    //         Math.expDigamma(
    //           thisStopBackoffATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisStopBackoffWATotal = logSum(
    //         stopBackoffWAInterpolationSums( backoffStopWAKey /*backoffStopWA*/ ),
    //         math.log( backoffWA_Alpha )
    //       )
    //       newStopBackoffWA_Score.setValue(
    //         h,
    //         Math.expDigamma(
    //           thisStopBackoffWATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisStopBackoffPATotal = logSum(
    //         stopBackoffPAInterpolationSums( backoffStopPAKey /*backoffStopPA*/ ),
    //         math.log( backoffPA_Alpha )
    //       )
    //       newStopBackoffPA_Score.setValue(
    //         h,
    //         Math.expDigamma(
    //           thisStopBackoffPATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //     }
    // 
    //     val stopBackoffDefaultDenom = Math.expDigamma(
    //       math.log(
    //         noBackoff_Alpha +
    //         backoffW_Alpha +
    //         backoffA_Alpha +
    //         backoffWA_Alpha +
    //         backoffPA_Alpha
    //       )
    //     )
    // 
    //     newNoStopBackoff_Score.setDefault(
    //       Math.expDigamma( math.log( noBackoff_Alpha ) ) -
    //         stopBackoffDefaultDenom
    //     )
    //     newStopBackoffW_Score.setDefault(
    //       Math.expDigamma( math.log( backoffW_Alpha ) ) -
    //         stopBackoffDefaultDenom
    //     )
    //     newStopBackoffA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffA_Alpha ) ) -
    //         stopBackoffDefaultDenom
    //     )
    //     newStopBackoffWA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffWA_Alpha ) ) -
    //         stopBackoffDefaultDenom
    //     )
    //     newStopBackoffPA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffPA_Alpha ) ) -
    //         stopBackoffDefaultDenom
    //     )
    // 
    // 
    // 
    //     // whew, now let's get interpolation parameters for chooseScore
    // 
    // 
    //     val chooseNoBackoffInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    //     val chooseBackoffWInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    //     val chooseBackoffAInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    //     val chooseBackoffWAInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    //     val chooseBackoffPAInterpolationSums = new Log1dTable( Set[ChooseArgument]() )
    // 
    //     val chooseBackoffInterpolationDenom = new Log1dTable( Set[ChooseArgument]() )
    // 
    //     // val chooseBackoffWInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    //     // val chooseBackoffAInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    //     // val chooseBackoffWAInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    //     // val chooseBackoffPAInterpolationTypeCounts = new Log1dTable( Set[ChooseArgument]() )
    // 
    //     // // We'll also be summing over the backoff terms to produce the tied backoff rules in this loop.
    //     val chooseNoBackoffCounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     val chooseBackoffWCounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     val chooseBackoffACounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     val chooseBackoffWACounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     val chooseBackoffPACounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    // 
    //     val chooseRecoverDepWSums = new Log2dTable( Set[ObservedLabel](), Set[Word]() )
    //     val chooseRecoverDepASums = new Log2dTable( Set[ObservedLabel](), Set[Word]() )
    //     val chooseRecoverDepWASums = new Log2dTable( Set[ObservedLabel](), Set[WordPair]() )
    //     val chooseRecoverDepPASums = new Log2dTable( Set[ObservedLabel](), Set[WordPair]() )
    // 
    //     // val chooseBackoffWSumsTypeCounts =
    //     //   new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     // val chooseBackoffASumsTypeCounts =
    //     //   new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     // val chooseBackoffWASumsTypeCounts =
    //     //   new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     // val chooseBackoffPASumsTypeCounts =
    //     //   new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    // 
    // 
    //     val rootChooseCounts =
    //       new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    // 
    //     //println( "Summing over chooseCounts to get counts for interpolation parameters and each backoff distribution." )
    //     chooseCounts.parents.foreach{ chooseKey =>
    //       chooseKey.h match {
    //         case WordTriple( hw, hp, ha ) => {
    //           val backoffHeadW = WordPair( hp, ha )
    //           val backoffHeadA = WordPair( hw, hp )
    //           val backoffHeadWA = Word( hp )
    //           val backoffHeadPA = Word( hw )
    // 
    //           val backoffHeadWKey = ChooseArgument( backoffHeadW, chooseKey.dir )
    //           val backoffHeadAKey = ChooseArgument( backoffHeadA, chooseKey.dir )
    //           val backoffHeadWAKey = ChooseArgument( backoffHeadWA, chooseKey.dir )
    //           val backoffHeadPAKey = ChooseArgument( backoffHeadPA, chooseKey.dir )
    // 
    //           //val backoffHeadKey = ChooseArgument( Word( h2 ), chooseKey.dir )
    // 
    //           chooseCounts(chooseKey).keySet.foreach{ arg =>
    //             arg match {
    //               case WordTriple( dw, dp, da ) => {
    // 
    //                 val fullInterpolation = WordSextuple( hw, hp, ha, dw, dp, da )
    //                 val fullInterpolationKey = ChooseArgument( fullInterpolation, chooseKey.dir )
    // 
    //                 val interpolationBackoffW = WordQuad( hp, ha, dp, da )
    //                 val interpolationBackoffA = WordQuad( hw, hp, dw, dp )
    //                 val interpolationBackoffWA = WordPair( hp, dp )
    //                 val interpolationBackoffPA = WordPair( hw, dw )
    // 
    //                 val interpolationBackoffWKey =
    //                   ChooseArgument( interpolationBackoffW, chooseKey.dir )
    //                 val interpolationBackoffAKey =
    //                   ChooseArgument( interpolationBackoffA, chooseKey.dir )
    //                 val interpolationBackoffWAKey =
    //                   ChooseArgument( interpolationBackoffWA, chooseKey.dir )
    //                 val interpolationBackoffPAKey =
    //                   ChooseArgument( interpolationBackoffPA, chooseKey.dir )
    // 
    //                 val backoffDepW = WordPair( dp, da )
    //                 val backoffDepA = WordPair( dw, dp )
    //                 val backoffDepWA = Word( dp )
    //                 val backoffDepPA = Word( dw )
    // 
    //                 val thisNoBackoffComponent =
    //                   chooseCounts( chooseKey, arg ) + noChooseBackoff_Score( fullInterpolationKey )
    //                 val thisBackoffWComponent =
    //                   chooseCounts( chooseKey, arg ) + chooseBackoffW_Score( fullInterpolationKey )
    //                 val thisBackoffAComponent =
    //                   chooseCounts( chooseKey, arg ) + chooseBackoffA_Score( fullInterpolationKey )
    //                 val thisBackoffWAComponent =
    //                   chooseCounts( chooseKey, arg ) + chooseBackoffWA_Score( fullInterpolationKey )
    //                 val thisBackoffPAComponent =
    //                   chooseCounts( chooseKey, arg ) + chooseBackoffPA_Score( fullInterpolationKey )
    // 
    //                 chooseNoBackoffInterpolationSums.setValue(
    //                   fullInterpolationKey,
    //                   logSum(
    //                     chooseNoBackoffInterpolationSums( fullInterpolationKey ),
    //                     thisNoBackoffComponent
    //                   )
    //                 )
    // 
    //                 chooseBackoffWInterpolationSums.setValue(
    //                   interpolationBackoffWKey,
    //                   logSum(
    //                     chooseBackoffWInterpolationSums( interpolationBackoffWKey ),
    //                     thisBackoffWComponent
    //                   )
    //                 )
    //                 // chooseBackoffWInterpolationTypeCounts.setValue(
    //                 //   interpolationBackoffWKey,
    //                 //   logSum(
    //                 //     chooseBackoffWInterpolationTypeCounts( interpolationBackoffWKey ), 0D
    //                 //   )
    //                 // )
    //                 chooseBackoffAInterpolationSums.setValue(
    //                   interpolationBackoffAKey,
    //                   logSum(
    //                     chooseBackoffAInterpolationSums( interpolationBackoffAKey ),
    //                     thisBackoffAComponent
    //                   )
    //                 )
    //                 // chooseBackoffAInterpolationTypeCounts.setValue(
    //                 //   interpolationBackoffAKey,
    //                 //   logSum(
    //                 //     chooseBackoffAInterpolationTypeCounts( interpolationBackoffAKey ), 0D
    //                 //   )
    //                 // )
    //                 chooseBackoffWAInterpolationSums.setValue(
    //                   interpolationBackoffWAKey,
    //                   logSum(
    //                     chooseBackoffWAInterpolationSums( interpolationBackoffWAKey ),
    //                     thisBackoffWAComponent
    //                   )
    //                 )
    //                 // chooseBackoffWAInterpolationTypeCounts.setValue(
    //                 //   interpolationBackoffWAKey,
    //                 //   logSum(
    //                 //     chooseBackoffWAInterpolationTypeCounts( interpolationBackoffWAKey ), 0D
    //                 //   )
    //                 // )
    //                 chooseBackoffPAInterpolationSums.setValue(
    //                   interpolationBackoffPAKey,
    //                   logSum(
    //                     chooseBackoffPAInterpolationSums( interpolationBackoffPAKey ),
    //                     thisBackoffPAComponent
    //                   )
    //                 )
    //                 // chooseBackoffPAInterpolationTypeCounts.setValue(
    //                 //   interpolationBackoffPAKey,
    //                 //   logSum(
    //                 //     chooseBackoffPAInterpolationTypeCounts( interpolationBackoffPAKey ), 0D
    //                 //   )
    //                 // )
    // 
    // 
    // 
    // 
    // 
    //                 // Also, count up for each backoff distribution (not the interpolation parameters).
    //                 chooseNoBackoffCounts.setValue(
    //                   chooseKey,
    //                   arg,
    //                   logSum(
    //                     chooseNoBackoffCounts( chooseKey, arg ),
    //                     thisNoBackoffComponent
    //                   )
    //                 )
    // 
    // 
    //                 chooseBackoffWCounts.setValue(
    //                   backoffHeadWKey,
    //                   backoffDepW,
    //                   logSum(
    //                     chooseBackoffWCounts( backoffHeadWKey, backoffDepW ),
    //                     thisBackoffWComponent
    //                   )
    //                 )
    //                 // chooseBackoffWSumsTypeCounts.setValue(
    //                 //   backoffHeadWKey,
    //                 //   backoffDepW,
    //                 //   logSum(
    //                 //     chooseBackoffWSumsTypeCounts( backoffHeadWKey, backoffDepW ),
    //                 //     0D
    //                 //   )
    //                 // )
    // 
    //                 chooseBackoffACounts.setValue(
    //                   backoffHeadAKey,
    //                   backoffDepA,
    //                   logSum(
    //                     chooseBackoffACounts( backoffHeadAKey, backoffDepA ),
    //                     thisBackoffAComponent
    //                   )
    //                 )
    //                 // chooseBackoffASumsTypeCounts.setValue(
    //                 //   backoffHeadAKey,
    //                 //   backoffDepA,
    //                 //   logSum(
    //                 //     chooseBackoffASumsTypeCounts( backoffHeadAKey, backoffDepA ),
    //                 //     0D
    //                 //   )
    //                 // )
    // 
    //                 chooseBackoffWACounts.setValue(
    //                   backoffHeadWAKey,
    //                   backoffDepWA,
    //                   logSum(
    //                     chooseBackoffWACounts( backoffHeadWAKey, backoffDepWA ),
    //                     thisBackoffWAComponent
    //                   )
    //                 )
    //                 // chooseBackoffWASumsTypeCounts.setValue(
    //                 //   backoffHeadWAKey,
    //                 //   backoffDepWA,
    //                 //   logSum(
    //                 //     chooseBackoffWASumsTypeCounts( backoffHeadWAKey, backoffDepWA ),
    //                 //     0D
    //                 //   )
    //                 // )
    // 
    //                 chooseBackoffPACounts.setValue(
    //                   backoffHeadPAKey,
    //                   backoffDepPA,
    //                   logSum(
    //                     chooseBackoffPACounts( backoffHeadPAKey, backoffDepPA ),
    //                     thisBackoffPAComponent
    //                   )
    //                 )
    //                 // chooseBackoffPASumsTypeCounts.setValue(
    //                 //   backoffHeadPAKey,
    //                 //   backoffDepPA,
    //                 //   logSum(
    //                 //     chooseBackoffPASumsTypeCounts( backoffHeadPAKey, backoffDepPA ),
    //                 //     0D
    //                 //   )
    //                 // )
    // 
    //               }
    //               case rootArg:AbstractRoot => {
    //                 rootChooseCounts.setValue(
    //                   chooseKey,
    //                   arg,
    //                   logSum(
    //                     rootChooseCounts( chooseKey, arg ),
    //                     chooseCounts( chooseKey, arg )
    //                   )
    //                 )
    //               }
    //             }
    //           }
    //         }
    //         case rootHead:AbstractRoot => {
    //           chooseCounts(chooseKey).keySet.foreach{ arg =>
    //             rootChooseCounts.setValue(
    //               chooseKey,
    //               arg,
    //               logSum(
    //                 rootChooseCounts( chooseKey, arg ),
    //                 chooseCounts( chooseKey, arg )
    //               )
    //             )
    //           }
    //         }
    //       }
    //     }
    // 
    // 
    //     /*
    //     // bring tied rules to average:
    //         // chooseBackoffWInterpolationSums.domain.foreach{ backoffW =>
    //         //   chooseBackoffWInterpolationSums.setValue(
    //         //     backoffW,
    //         //     chooseBackoffWInterpolationSums( backoffW ) -
    //         //       chooseBackoffWInterpolationTypeCounts( backoffW )
    //         //   )
    //         // }
    //         // chooseBackoffAInterpolationSums.domain.foreach{ backoffA =>
    //         //   chooseBackoffAInterpolationSums.setValue(
    //         //     backoffA,
    //         //     chooseBackoffAInterpolationSums( backoffA ) -
    //         //       chooseBackoffAInterpolationTypeCounts( backoffA )
    //         //   )
    //         // }
    //         // chooseBackoffWAInterpolationSums.domain.foreach{ backoffWA =>
    //         //   chooseBackoffWAInterpolationSums.setValue(
    //         //     backoffWA,
    //         //     chooseBackoffWAInterpolationSums( backoffWA ) -
    //         //       chooseBackoffWAInterpolationTypeCounts( backoffWA )
    //         //   )
    //         // }
    //         // chooseBackoffPAInterpolationSums.domain.foreach{ backoffPA =>
    //         //   chooseBackoffPAInterpolationSums.setValue(
    //         //     backoffPA,
    //         //     chooseBackoffPAInterpolationSums( backoffPA ) -
    //         //       chooseBackoffPAInterpolationTypeCounts( backoffPA )
    //         //   )
    //         // }
    // 
    // 
    // 
    //     chooseBackoffWCounts.parents.foreach{ backoffHeadWKey =>
    //       chooseBackoffWCounts(backoffHeadWKey).keySet.foreach{ backoffDepW =>
    //         chooseBackoffWCounts.setValue(
    //           backoffHeadWKey,
    //           backoffDepW,
    //           chooseBackoffWCounts( backoffHeadWKey, backoffDepW ) -
    //             chooseBackoffWSumsTypeCounts( backoffHeadWKey, backoffDepW )
    //         )
    //       }
    //     }
    //     chooseBackoffACounts.parents.foreach{ backoffHeadAKey =>
    //       chooseBackoffACounts(backoffHeadAKey).keySet.foreach{ backoffDepA =>
    //         chooseBackoffACounts.setValue(
    //           backoffHeadAKey,
    //           backoffDepA,
    //           chooseBackoffACounts( backoffHeadAKey, backoffDepA ) -
    //             chooseBackoffASumsTypeCounts( backoffHeadAKey, backoffDepA )
    //         )
    //       }
    //     }
    //     chooseBackoffWACounts.parents.foreach{ backoffHeadWAKey =>
    //       chooseBackoffWACounts(backoffHeadWAKey).keySet.foreach{ backoffDepWA =>
    //         chooseBackoffWACounts.setValue(
    //           backoffHeadWAKey,
    //           backoffDepWA,
    //           chooseBackoffWACounts( backoffHeadWAKey, backoffDepWA ) -
    //             chooseBackoffWASumsTypeCounts( backoffHeadWAKey, backoffDepWA )
    //         )
    //       }
    //     }
    //     chooseBackoffPACounts.parents.foreach{ backoffHeadPAKey =>
    //       chooseBackoffPACounts(backoffHeadPAKey).keySet.foreach{ backoffDepPA =>
    //         chooseBackoffPACounts.setValue(
    //           backoffHeadPAKey,
    //           backoffDepPA,
    //           chooseBackoffPACounts( backoffHeadPAKey, backoffDepPA ) -
    //             chooseBackoffPASumsTypeCounts( backoffHeadPAKey, backoffDepPA )
    //         )
    //       }
    //     }
    //     */
    // 
    //     //println( "Finishing up sums for choose denominator." )
    //     //chooseBackoffInterpolationDenom.domain.foreach{ denom =>
    //     chooseNoBackoffInterpolationSums.domain.foreach{ denom =>
    //       val WordSextuple( hw, hp, ha, dw, dp, da ) = denom.h
    //       val interpolationBackoffW = WordQuad( hp, ha, dp, da )
    //       val interpolationBackoffA = WordQuad( hw, hp, dw, dp )
    //       val interpolationBackoffWA = WordPair( hp, dp )
    //       val interpolationBackoffPA = WordPair( hw, dw )
    // 
    //       val attachDir = denom.dir
    // 
    //       val interpolationBackoffWKey =
    //         ChooseArgument( interpolationBackoffW, attachDir )
    //       val interpolationBackoffAKey =
    //         ChooseArgument( interpolationBackoffA, attachDir )
    //       val interpolationBackoffWAKey =
    //         ChooseArgument( interpolationBackoffWA, attachDir )
    //       val interpolationBackoffPAKey =
    //         ChooseArgument( interpolationBackoffPA, attachDir )
    // 
    // 
    //       chooseBackoffInterpolationDenom.setValue(
    //         denom,
    //         logSum(
    //           Seq(
    //             //chooseBackoffInterpolationDenom( denom ),
    //             chooseNoBackoffInterpolationSums( interpolationBackoffWKey ),
    //             chooseBackoffWInterpolationSums( interpolationBackoffWKey ),
    //             chooseBackoffAInterpolationSums( interpolationBackoffAKey ),
    //             chooseBackoffWAInterpolationSums( interpolationBackoffWAKey ),
    //             chooseBackoffPAInterpolationSums( interpolationBackoffPAKey ),
    //             math.log(
    //               noBackoff_Alpha +
    //               backoffW_Alpha +
    //               backoffA_Alpha +
    //               backoffWA_Alpha +
    //               backoffPA_Alpha
    //             )
    //           )
    //         )
    //       )
    //     }
    // 
    //     val newNoChooseBackoff_Score = new Log1dTable( Set[ChooseArgument]() )
    //     val newChooseBackoffW_Score = new Log1dTable( Set[ChooseArgument]() )
    //     val newChooseBackoffA_Score = new Log1dTable( Set[ChooseArgument]() )
    //     val newChooseBackoffWA_Score = new Log1dTable( Set[ChooseArgument]() )
    //     val newChooseBackoffPA_Score = new Log1dTable( Set[ChooseArgument]() )
    // 
    //     //println( "Estimating new choose interpolation distributions." )
    //     chooseBackoffInterpolationDenom.domain.foreach{ fullInterpolationKey =>
    //       val WordSextuple( hw, hp, ha, dw, dp, da ) = fullInterpolationKey.h
    // 
    //       val interpolationBackoffW = WordQuad( hp, ha, dp, da )
    //       val interpolationBackoffA = WordQuad( hw, hp, dw, dp )
    //       val interpolationBackoffWA = WordPair( hp, dp )
    //       val interpolationBackoffPA = WordPair( hw, dw )
    // 
    //       val attachDir = fullInterpolationKey.dir
    //       val interpolationBackoffWKey =
    //         ChooseArgument( interpolationBackoffW, attachDir )
    //       val interpolationBackoffAKey =
    //         ChooseArgument( interpolationBackoffA, attachDir )
    //       val interpolationBackoffWAKey =
    //         ChooseArgument( interpolationBackoffWA, attachDir )
    //       val interpolationBackoffPAKey =
    //         ChooseArgument( interpolationBackoffPA, attachDir )
    // 
    // 
    //       val expDigammaDenom = Math.expDigamma( chooseBackoffInterpolationDenom( fullInterpolationKey ) )
    // 
    //       val thisChooseNoBackoffTotal = logSum(
    //         chooseNoBackoffInterpolationSums( fullInterpolationKey ),
    //         math.log( noBackoff_Alpha )
    //       )
    //       newNoChooseBackoff_Score.setValue(
    //         fullInterpolationKey,
    //         Math.expDigamma(
    //           thisChooseNoBackoffTotal
    //         ) - expDigammaDenom
    //       )
    // 
    // 
    //       val thisChooseBackoffWTotal = logSum(
    //         chooseBackoffWInterpolationSums( interpolationBackoffWKey ),
    //         math.log( backoffW_Alpha )
    //       )
    //       newChooseBackoffW_Score.setValue(
    //         fullInterpolationKey,
    //         Math.expDigamma(
    //           thisChooseBackoffWTotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisChooseBackoffATotal = logSum(
    //         chooseBackoffAInterpolationSums( interpolationBackoffAKey ),
    //         math.log( backoffA_Alpha )
    //       )
    //       newChooseBackoffA_Score.setValue(
    //         fullInterpolationKey,
    //         Math.expDigamma(
    //           thisChooseBackoffATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisChooseBackoffWATotal = logSum(
    //         chooseBackoffWAInterpolationSums( interpolationBackoffWAKey ),
    //         math.log( backoffWA_Alpha )
    //       )
    //       newChooseBackoffWA_Score.setValue(
    //         fullInterpolationKey,
    //         Math.expDigamma(
    //           thisChooseBackoffWATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //       val thisChooseBackoffPATotal = logSum(
    //         chooseBackoffPAInterpolationSums( interpolationBackoffPAKey ),
    //         math.log( backoffPA_Alpha )
    //       )
    //       newChooseBackoffPA_Score.setValue(
    //         fullInterpolationKey,
    //         Math.expDigamma(
    //           thisChooseBackoffPATotal
    //         ) - expDigammaDenom
    //       )
    // 
    //     }
    // 
    //     val chooseBackoffDefaultDenom = Math.expDigamma(
    //       math.log(
    //         noBackoff_Alpha +
    //         backoffW_Alpha +
    //         backoffA_Alpha +
    //         backoffWA_Alpha +
    //         backoffPA_Alpha
    //       )
    //     )
    // 
    //     newNoChooseBackoff_Score.setDefault(
    //       Math.expDigamma( math.log( noBackoff_Alpha ) ) -
    //         chooseBackoffDefaultDenom
    //     )
    //     newChooseBackoffW_Score.setDefault(
    //       Math.expDigamma( math.log( backoffW_Alpha ) ) -
    //         chooseBackoffDefaultDenom
    //     )
    //     newChooseBackoffA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffA_Alpha ) ) -
    //         chooseBackoffDefaultDenom
    //     )
    //     newChooseBackoffWA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffWA_Alpha ) ) -
    //         chooseBackoffDefaultDenom
    //     )
    //     newChooseBackoffPA_Score.setDefault(
    //       Math.expDigamma( math.log( backoffPA_Alpha ) ) -
    //         chooseBackoffDefaultDenom
    //     )
    // 
    // 
    // 
    //     // Ok, now compute backed-off parameters
    //     //println( "Estimating new stop distributions with backoff." )
    // 
    //     stopNoBackoffCounts.expDigammaNormalize()
    //     stopBackoffWCounts.expDigammaNormalize()
    //     stopBackoffACounts.expDigammaNormalize()
    //     stopBackoffWACounts.expDigammaNormalize()
    //     stopBackoffPACounts.expDigammaNormalize()
    // 
    //     val backedoffStop = new Log2dTable( Set[StopOrNot](), dmv.stopDecision )
    //     stopCounts.parents.foreach{ stopKey =>
    //       dmv.stopDecision.foreach{ stopDecision =>
    //         stopKey.w match {
    //           case WordTriple( hw, hp, ha ) => {
    // 
    //             val backoffStopW = WordPair( hp, ha )
    //             val backoffStopA = WordPair( hw, hp )
    //             val backoffStopWA = Word( hp )
    //             val backoffStopPA = Word( hw )
    // 
    //             val backoffStopWKey = StopOrNot( backoffStopW , stopKey.dir, stopKey.adj )
    //             val backoffStopAKey = StopOrNot( backoffStopA , stopKey.dir, stopKey.adj )
    //             val backoffStopWAKey = StopOrNot( backoffStopWA , stopKey.dir, stopKey.adj )
    //             val backoffStopPAKey = StopOrNot( backoffStopPA , stopKey.dir, stopKey.adj )
    // 
    //             backedoffStop.setValue(
    //               stopKey,
    //               stopDecision,
    //               logSum(
    //                 newNoStopBackoff_Score( stopKey /*stopKey.w*/ ) +
    //                   stopNoBackoffCounts( stopKey, stopDecision ),
    //                 newStopBackoffW_Score( stopKey /*stopKey.w*/ ) +
    //                   stopBackoffWCounts( backoffStopWKey, stopDecision),
    //                 newStopBackoffA_Score( stopKey /*stopKey.w*/ ) +
    //                   stopBackoffACounts( backoffStopAKey, stopDecision),
    //                 newStopBackoffWA_Score( stopKey /*stopKey.w*/ ) +
    //                   stopBackoffWACounts( backoffStopWAKey, stopDecision),
    //                 newStopBackoffPA_Score( stopKey /*stopKey.w*/ ) +
    //                   stopBackoffPACounts( backoffStopPAKey, stopDecision)
    //               )
    //             )
    // 
    //           }
    //           case rootHead:AbstractRoot => {
    //             backedoffStop.setValue(
    //               stopKey,
    //               stopDecision,
    //               rootStopCounts( stopKey, stopDecision )
    //             )
    //           }
    //         }
    //       }
    //     }
    // 
    //     backedoffStop.setDefault(
    //       Math.expDigamma( 0D ) - Math.expDigamma( math.log( backedoffStop.parents.size ) )
    //     )
    // 
    // 
    //     chooseNoBackoffCounts.expDigammaNormalize()
    //     chooseBackoffWCounts.expDigammaNormalize()
    //     chooseBackoffACounts.expDigammaNormalize()
    //     chooseBackoffWACounts.expDigammaNormalize()
    //     chooseBackoffPACounts.expDigammaNormalize()
    // 
    //     chooseRecoverDepWSums.expDigammaNormalize()
    //     chooseRecoverDepASums.expDigammaNormalize()
    //     chooseRecoverDepWASums.expDigammaNormalize()
    //     chooseRecoverDepPASums.expDigammaNormalize()
    // 
    //     rootChooseCounts.expDigammaNormalize()
    // 
    //     val chooseDefaults = collection.mutable.Map[ChooseArgument,Double]().withDefaultValue(
    //       Math.expDigamma( 0 ) - Math.expDigamma( math.log( chooseNoBackoffCounts.parents.size ) )
    //     )
    // 
    //     val backedoffChoose = new Log2dTable( Set[ChooseArgument](), Set[ObservedLabel]() )
    //     chooseCounts.parents.foreach{ chooseKey =>
    // 
    //       chooseCounts(chooseKey).keySet.foreach{ arg =>
    //         chooseKey.h match {
    //           case WordTriple( hw, hp, ha ) => {
    //             val backoffHeadW = WordPair( hp, ha )
    //             val backoffHeadA = WordPair( hw, hp )
    //             val backoffHeadWA = Word( hp )
    //             val backoffHeadPA = Word( hw )
    // 
    //             val backoffHeadWKey = ChooseArgument( backoffHeadW, chooseKey.dir )
    //             val backoffHeadAKey = ChooseArgument( backoffHeadA, chooseKey.dir )
    //             val backoffHeadWAKey = ChooseArgument( backoffHeadWA, chooseKey.dir )
    //             val backoffHeadPAKey = ChooseArgument( backoffHeadPA, chooseKey.dir )
    // 
    // 
    // 
    //             arg match {
    //               case WordTriple( dw, dp, da ) => {
    // 
    //                 val interpolation = WordSextuple( hw, hp, ha, dw, dp, da )
    //                 val interpolationKey = ChooseArgument( interpolation, chooseKey.dir )
    // 
    //                 val backoffDepW = WordPair( dp, da )
    //                 val backoffDepA = WordPair( dw, dp )
    //                 val backoffDepWA = Word( dp )
    //                 val backoffDepPA = Word( dw )
    // 
    //                 val wToRecover = Word( dw )
    //                 val aToRecover = Word( da )
    //                 val waToRecover = WordPair( dw, da )
    //                 val paToRecover = WordPair( dp, da )
    // 
    //                 chooseDefaults +=
    //                   chooseKey ->
    //                     logSum(
    //                       Seq(
    //                         newNoChooseBackoff_Score.getDefault +
    //                           chooseNoBackoffCounts.getDefault( chooseKey ),
    //                         newChooseBackoffW_Score.getDefault +
    //                           chooseBackoffWCounts.getDefault( backoffHeadWKey ),
    //                         newChooseBackoffA_Score.getDefault +
    //                           chooseBackoffACounts.getDefault( backoffHeadAKey ),
    //                         newChooseBackoffWA_Score.getDefault +
    //                           chooseBackoffWACounts.getDefault( backoffHeadWAKey ),
    //                         newChooseBackoffPA_Score.getDefault +
    //                           chooseBackoffPACounts.getDefault( backoffHeadPAKey )
    //                       )
    //                     )
    // 
    //                 backedoffChoose.setValue(
    //                   chooseKey,
    //                   arg,
    //                   logSum(
    //                     Seq(
    //                       newNoChooseBackoff_Score( interpolationKey ) +
    //                         chooseNoBackoffCounts( chooseKey, arg ),
    //                       newChooseBackoffW_Score( interpolationKey ) +
    //                         chooseBackoffWCounts( backoffHeadWKey, backoffDepW ) +
    //                           chooseRecoverDepWSums( backoffHeadW, wToRecover ),
    //                       newChooseBackoffA_Score( interpolationKey ) +
    //                         chooseBackoffACounts( backoffHeadAKey, backoffDepA ) +
    //                           chooseRecoverDepASums( backoffHeadA, aToRecover ),
    //                       newChooseBackoffWA_Score( interpolationKey ) +
    //                         chooseBackoffWACounts( backoffHeadWAKey, backoffDepWA ) +
    //                           chooseRecoverDepWASums( backoffHeadWA, waToRecover ),
    //                       newChooseBackoffPA_Score( interpolationKey ) +
    //                         chooseBackoffPACounts( backoffHeadPAKey, backoffDepPA ) +
    //                           chooseRecoverDepPASums( backoffHeadPA, paToRecover )
    //                     )
    //                   )
    //                 )
    //               }
    //               case rootArg:AbstractRoot => {
    //                 backedoffChoose.setValue(
    //                   chooseKey,
    //                   arg,
    //                   Double.NegativeInfinity
    //                 )
    //               }
    //             }
    //           }
    //           case rootHead:AbstractRoot => {
    //             backedoffChoose.setValue(
    //               chooseKey,
    //               arg,
    //               rootChooseCounts( chooseKey, arg )
    //             )
    //           }
    //         }
    //       }
    //     }
    // 
    //     backedoffChoose.setDefault(
    //       Math.expDigamma( 0D ) - Math.expDigamma( math.log( backedoffChoose.parents.size ) )
    //     )
    //     backedoffChoose.setDefaultMap( chooseDefaults )
    // 
    //     // println( "Testing choose default in partial counts (" + backedoffChoose.parents.size + "):  " +
    //     //   backedoffChoose( ChooseArgument( Word( "yugioh" ), RightAttachment ) , Word( "yohoho" ) ) +
    //     //   "\t" + backedoffChoose( ChooseArgument( WordPair( "CC", "6" ), RightAttachment ) , Word( "yohoho" ) ) +
    //     //   "\t" + 
    //     //     backedoffChoose( ChooseArgument( WordPair( "CC", "6" ), RightAttachment ) , WordPair( "IN", "2" ) )
    //     // )
    // 
    //     // assert(
    //     //   backedoffStop.parents.forall( stopKey =>
    //     //     logSum( backedoffStop(stopKey).values.toSeq ) <= 0D
    //     //   )
    //     // )
    //     // assert(
    //     //   backedoffChoose.parents.forall( chooseKey =>
    //     //     logSum( backedoffChoose(chooseKey).values.toSeq ) <= 0D
    //     //   )
    //     // )
    // 
    //     //println( "normalizing stop backoff distribution." )
    //     //backedoffStop.expDigammaNormalize()
    //     //println( "normalizing choose backoff distribution." )
    //     //backedoffChoose.expDigammaNormalize()
    // 
    //     println( "Done!" )
    // 
    //     // val toReturn = new DMVBayesianBackoffPartialCounts(
    //     //   noChooseBackoff,
    //     //   backoffArg,
    //     //   backoffBoth,
    //     //   noStopBackoff,
    //     //   stopBackoff,
    //     //   newNoChooseBackoffScore,
    //     //   newChooseBackoffArgScore,
    //     //   newChooseBackoffBothScore,
    //     //   newNoStopBackoffScore,
    //     //   newStopBackoffScore
    //     // )
    // 
    //     //println( "about to create new grammar with newNoStopBackoffScore:\n" + newNoStopBackoffScore )
    //     val toReturn = associatedGrammar
    // 
    //     // val freeEnergy =
    //     //   freeEnergyElementOne + freeEnergyElementTwo - freeEnergyElementThree + freeEnergyElementFour
    // 
    //     // println( "Free energy is: " +
    //     //   freeEnergyElementOne + " + " + freeEnergyElementTwo + " - " + freeEnergyElementThree + " + " +
    //     //   freeEnergyElementFour + " = " + freeEnergy )
    // 
    //     toReturn.setParams(
    //       DMVBayesianBackoffSimpleThreeStreamParameters(
    //         orderCounts.toLogCPT,
    //         backedoffStop.asLogCPT,
    //         backedoffChoose.asLogCPT,
    //         newNoStopBackoff_Score,
    //         newStopBackoffW_Score,
    //         newStopBackoffA_Score,
    //         newStopBackoffWA_Score,
    //         newStopBackoffPA_Score,
    //         newNoChooseBackoff_Score,
    //         newChooseBackoffW_Score,
    //         newChooseBackoffA_Score,
    //         newChooseBackoffWA_Score,
    //         newChooseBackoffPA_Score
    //       )
    //     )
    // 
    //     toReturn
    // 
    //   }
    // 
    //     /// override def toString =
    //     ///   super.toString +
    //     ///     "Alphas:\n" +
    //     ///     "\tnoStopBackoffAlpha: " + noStopBackoff + "\n" +
    //     ///     "\tstopBackoffAlpha: " + stopBackoff + "\n" +
    //     ///     "\tnoChooseBackoffAlpha: " + noChooseBackoff + "\n" +
    //     ///     "\tbackoffHeadAlpha: " + backoffHead + "\n" +
    //     ///     "\tbackoffArgAlpha: " + backoffArg + "\n" +
    //     ///     "\tbackoffBothAlpha: " + backoffBoth + "\n"
    // 
    // 
    // }
    // 
