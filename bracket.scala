package chessOS

// import scala.util.Random

// class Player(
//     val name: String
// ) {
//     var doubleScore:   Int = 0
//     var randomSeed:    Int = 0
//     var monradRanking: Int = 0
//     var numWhiteGames: Int = 0
//     var numBlackGames: Int = 0

//     override def equals(that: Any): Boolean = {
//         that match
//             case that: Player => this.name == that.name
//             case _ => false
//     }

//     override def hashCode: Int = name.hashCode

//     override def toString: String = {
//         s"[$name], score: $score, prev white games: $numWhiteGames, prev black games: $numBlackGames\n"
//     }
// }

// def samplePlayerList: Seq[Player] = {
//     Seq(
//         Player("A"),
//         Player("B"),
//         Player("C"),
//         Player("D"),
//         Player("E"),
//         Player("F"),
//         Player("G"),
//         Player("H"),
//         Player("I")
//     )
// }

// def assertNotTooManyPlayers(playerList: Seq[Player]): Unit = {
//     if (playerList.size > 9)
//         println("Too many players. Stopping.")
//         throw new IllegalArgumentException
// }

// def assignRandomSeeds(playerList: Seq[Player]): Unit = {
//     Random.shuffle(playerList).zipWithIndex.map{
//         case (p, i) => p.randomSeed = i
//     }
// }

// def assignMonradRankings(playerList: Seq[Player]): Unit = {
//     playerList.sortBy(p => (p.score, p.randomSeed)).zipWithIndex.map{
//         case (p, i) => p.monradRanking = i
//     }
// }

// def assertNotTooManyRounds(playerList: Seq[Player], numRounds: Int): Unit = {
//     if (playerList.size < numRounds)
//         println("More rounds than participants. Stopping.")
//         throw new IllegalArgumentException
// }

// def runBracket = {

//     var previousPairings = Set.empty[Pairing]

//     val numRounds = 9

//     val playerList = samplePlayerList
//     assertNotTooManyPlayers(playerList)
//     assertNotTooManyRounds(playerList, numRounds)
//     assignRandomSeeds(playerList)

//     for
//         round <- (1 to numRounds)
//     do
//         println(s"Start of round $round")
//         assignMonradRankings(playerList)

//         val possiblePairings = generatePairings(playerList)

//         val legalPairings = filterIllegalPairings(possiblePairings, previousPairings)

//         val chosenPairingList = selectOptimalPairing(legalPairings)

//         println(s"Pairings: $chosenPairingList")

//         previousPairings ++= chosenPairingList.toSet

//         updateColorCounts(chosenPairingList)

//         // update the scores somehow
//         chosenPairingList.foreach{
//             case Pairing.Game(white, black) => white.score += 1
//             case Pairing.Bye(_) => {}
//         }

//         chosenPairingList.foreach{
//             case Pairing.Game(_, _) => {}
//             case Pairing.Bye(p) => p.score += 1
//         }

//         scala.io.StdIn.readLine
// }

// def generatePairings(playerList: Seq[Player]): Seq[Seq[Pairing]] = {
//     if (playerList.size % 2 == 0)
//         evenPairings(playerList)
//     else
//         playerList.flatMap(byePlayer =>
//             val remainingPlayers = playerList.filter(_ != byePlayer)
//             evenPairings(remainingPlayers).map(pairingList =>
//                 Pairing.Bye(byePlayer) +: pairingList
//             )
//         )
// }

// def evenPairings(playerList: Seq[Player]): Seq[Seq[Pairing]] = {
//     if (playerList.size == 2) {
//         Seq(
//             Seq(Pairing.Game(playerList(0), playerList(1))),
//             Seq(Pairing.Game(playerList(1), playerList(0)))
//         )
//     } else {
//         playerList.flatMap(playerOne =>
//             val remainingOne = playerList.filter(_ != playerOne)
//             remainingOne.flatMap(playerTwo =>
//                 val remainingTwo = remainingOne.filter(_ != playerTwo)
//                 evenPairings(remainingTwo).flatMap(pairingList =>
//                     Seq(
//                         Pairing.Game(playerOne, playerTwo) +: pairingList,
//                         Pairing.Game(playerTwo, playerOne) +: pairingList,
//                     )
//                 )
//             )
//         )
//     }
// }

// def filterIllegalPairings(possible: Seq[Seq[Pairing]], previous: Set[Pairing]): Seq[Seq[Pairing]] = {

//     possible
//         .filter(pairingList =>
//             val proposedBye = pairingList.filter{
//                 case _: Pairing.Bye => true
//                 case _: Pairing.Game => false
//             }
//             if (proposedBye.size == 0)
//                 true
//             else
//                 !previous.contains(proposedBye(0))
//         )
//         .filter(pairingList =>
//             val proposedGames = pairingList.filter{
//                 case _: Pairing.Game => true
//                 case _: Pairing.Bye => false
//             }.map{
//                 case Pairing.Game(white, black) => Set(white, black)
//                 case _ => Set.empty[Player]
//             }.toSet
            
//             val previousGames = previous.filter{
//                 case _: Pairing.Game => true
//                 case _: Pairing.Bye => false
//             }.map{
//                 case Pairing.Game(white, black) => Set(white, black)
//                 case _ => Set.empty[Player]
//             }

//             proposedGames.intersect(previousGames).size == 0
//         )
//         .filter(pairingList =>
//             pairingList.forall{
//                 case Pairing.Bye(_) => true
//                 case Pairing.Game(white, black) =>
//                     (white.numWhiteGames - white.numBlackGames < 2 &&
//                     black.numBlackGames - black.numWhiteGames < 2)
//             }
//         )
// }

// def selectOptimalPairing(legalPairings: Seq[Seq[Pairing]]): Seq[Pairing] = {

//     legalPairings.sortBy(pairingList =>
//         val byeScore = {
//             pairingList.map{
//                 case Pairing.Bye(p) => p.score
//                 case Pairing.Game(_, _) => 0
//             }.sum
//         }
//         val monradScore = {
//             pairingList.map{
//                 case Pairing.Bye(_) => 0
//                 case Pairing.Game(white, black) =>
//                     val gap = (white.monradRanking - black.monradRanking).abs - 1
//                     gap * gap
//             }.sum
//         }
//         val colorScore = {
//             pairingList.map{
//                 case Pairing.Bye(_) => 0
//                 case Pairing.Game(white, black) =>
//                     (white.numWhiteGames - white.numBlackGames +
//                     black.numBlackGames - black.numWhiteGames)
//             }.sum
//         }
//         (byeScore, monradScore, colorScore)
//     ).head
// }

// def updateColorCounts(chosenPairingList: Seq[Pairing]): Unit = {
//     chosenPairingList.foreach{
//         case Pairing.Bye(_) => {}
//         case Pairing.Game(white, black) =>
//             white.numWhiteGames += 1
//             black.numBlackGames += 1
//     }
// }