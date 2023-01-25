import scala.util.Random

class Player(
    val name: String
) {
    var score:         Int = 0
    var randomSeed:    Int = 0
    var monradRanking: Int = 0
    var numWhiteGames: Int = 0
    var numBlackGames: Int = 0

    override def equals(that: Any): Boolean = {
        that match
            case that: Player => this.name == that.name
            case _ => false
    }

    override def hashCode: Int = name.hashCode

    override def toString: String = name
}

enum Pairing:
    case Game(white: Player, black: Player)
    case Bye(player: Player)

def samplePlayerList: Seq[Player] = {
    Seq(
        Player("A"),
        Player("B"),
        Player("C"),
        Player("D"),
        Player("E"),
        Player("F"),
        Player("G")
    )
}

def assertNotTooManyPlayers(playerList: Seq[Player]): Unit = {
    if (playerList.size > 9)
        println("Too many players. Stopping.")
        throw new IllegalArgumentException
}

def assertNoDuplicateNames(playerList: Seq[Player]): Unit = {
    val names = playerList.map(_.name)
    if (names.size != names.toSet.size)
        println("Duplicate names detected. Stopping.")
        throw new IllegalArgumentException
}

def assignRandomSeeds(playerList: Seq[Player]): Unit = {
    Random.shuffle(playerList).zipWithIndex.map{
        case (p, i) => p.randomSeed = i
    }
}

def assignMonradRankings(playerList: Seq[Player]): Unit = {
    playerList.sortBy(p => (p.score, p.randomSeed)).zipWithIndex.map{
        case (p, i) => p.monradRanking = i
    }
}

@main def main = {

    val playerList = samplePlayerList
    assertNotTooManyPlayers(playerList)
    assertNoDuplicateNames(playerList)
    assignRandomSeeds(playerList)

    var previousPairings = Set.empty[Pairing]

    val numRounds = 4
    for
        round <- (1 to numRounds)
    do
        assignMonradRankings(playerList)

        val possiblePairings = generatePairings(playerList)

        val legalPairings = filterIllegalPairings(possiblePairings, previousPairings)

        val chosenPairingList = selectOptimalPairing(legalPairings)
        
        println(s"Pairings: $chosenPairingList")

        previousPairings ++= chosenPairingList.toSet

        updateColorCounts(chosenPairingList)

        // update the scores somehow

        chosenPairingList.foreach{
            case Pairing.Game(_, _) => {}
            case Pairing.Bye(p) => p.score += 1
        }
}

def generatePairings(playerList: Seq[Player]): Seq[Seq[Pairing]] = {
    if (playerList.size % 2 == 0)
        evenPairings(playerList)
    else
        playerList.flatMap(byePlayer =>
            val remainingPlayers = playerList.filter(_ != byePlayer)
            evenPairings(remainingPlayers).map(pairingList =>
                Pairing.Bye(byePlayer) +: pairingList
            )
        )
}

def evenPairings(playerList: Seq[Player]): Seq[Seq[Pairing]] = {
    if (playerList.size == 2) {
        Seq(
            Seq(Pairing.Game(playerList(0), playerList(1))),
            Seq(Pairing.Game(playerList(1), playerList(0)))
        )
    } else {
        playerList.flatMap(playerOne =>
            val remainingOne = playerList.filter(_ != playerOne)
            remainingOne.flatMap(playerTwo =>
                val remainingTwo = remainingOne.filter(_ != playerTwo)
                evenPairings(remainingTwo).flatMap(pairingList =>
                    Seq(
                        Pairing.Game(playerOne, playerTwo) +: pairingList,
                        Pairing.Game(playerTwo, playerOne) +: pairingList,
                    )
                )
            )
        )
    }
}

def filterIllegalPairings(possible: Seq[Seq[Pairing]], previous: Set[Pairing]): Seq[Seq[Pairing]] = {

    possible
        .filter(pairingList =>
            val proposedBye = pairingList.filter{
                case _: Pairing.Bye => true
                case _: Pairing.Game => false
            }
            if (proposedBye.size == 0)
                true
            else
                !previous.contains(proposedBye(0))
        )
        .filter(pairingList =>
            val proposedGames = pairingList.filter{
                case _: Pairing.Game => true
                case _: Pairing.Bye => false
            }.map{
                case Pairing.Game(white, black) => Set(white, black)
                case _ => Set.empty[Player]
            }.toSet
            
            val previousGames = previous.filter{
                case _: Pairing.Game => true
                case _: Pairing.Bye => false
            }.map{
                case Pairing.Game(white, black) => Set(white, black)
                case _ => Set.empty[Player]
            }

            proposedGames.intersect(previousGames).size == 0
        )
        .filter(pairingList =>
            pairingList.forall{
                case Pairing.Bye(_) => true
                case Pairing.Game(white, black) =>
                    (white.numWhiteGames - white.numBlackGames < 2 &&
                    black.numBlackGames - black.numWhiteGames < 2)
            }
        )
}

def selectOptimalPairing(legalPairings: Seq[Seq[Pairing]]): Seq[Pairing] = {

    legalPairings.sortBy(pairingList =>
        val monradScore = {
            pairingList.map{
                case Pairing.Bye(_) => 0
                case Pairing.Game(white, black) =>
                    val gap = white.monradRanking - black.monradRanking
                    gap * gap
            }.sum
        }
        val colorScore = {
            pairingList.map{
                case Pairing.Bye(_) => 0
                case Pairing.Game(white, black) =>
                    (white.numWhiteGames - white.numBlackGames +
                    black.numBlackGames - black.numWhiteGames)
            }.sum
        }
        (monradScore, colorScore)
    ).head
}

def updateColorCounts(chosenPairingList: Seq[Pairing]): Unit = {
    chosenPairingList.foreach{
        case Pairing.Bye(_) => {}
        case Pairing.Game(white, black) =>
            white.numWhiteGames += 1
            black.numBlackGames += 1
    }
}