package chessOS

import scala.util.Random

enum BracketPairing:
    case Game(white: BracketPlayer, black: BracketPlayer)
    case Bye(player: BracketPlayer)

class BracketPlayer (
    val player: Player
) {
    var doubleScore:   Int = 0
    var randomSeed:    Int = 0
    var monradRanking: Int = 0
    var numWhiteGames: Int = 0
    var numBlackGames: Int = 0

    override def hashCode: Int = player.hashCode

    override def equals(that: Any): Boolean = {
        that match
            case that: BracketPlayer => player == that.player
            case _ => false
    }

    override def toString: String = {
        if (doubleScore == 2)
            s"${player} ${doubleScore/2} pt"
        else if (doubleScore % 2 == 0)
            s"${player} ${doubleScore/2} pts"
        else
            s"${player} ${doubleScore/2.0} pts"
    }
}

def assertNotTooManyPlayers(playerList: Seq[BracketPlayer]): Unit = {
    if (playerList.size > 9)
        println("Too many players. Stopping.")
        throw new IllegalArgumentException
}

def assignRandomSeeds(playerList: Seq[BracketPlayer]): Unit = {
    Random.shuffle(playerList).zipWithIndex.map{
        case (p, i) => p.randomSeed = i
    }
}

def assignMonradRankings(playerList: Seq[BracketPlayer]): Unit = {
    playerList.sortBy(p => (p.doubleScore, p.randomSeed)).zipWithIndex.map{
        case (p, i) => p.monradRanking = i
    }
}

def assertNotTooManyRounds(playerList: Seq[BracketPlayer], numRounds: Int): Unit = {
    if (playerList.size < numRounds)
        println("More rounds than participants. Stopping.")
        throw new IllegalArgumentException
}

def runBracket(signupList: Seq[Player], numRounds: Int) = {

    val playerList = signupList.map(p => BracketPlayer(p))
    assertNotTooManyPlayers(playerList)
    assertNotTooManyRounds(playerList, numRounds)
    assignRandomSeeds(playerList)

    var previousPairings = Set.empty[BracketPairing]

    for
        round <- (1 to numRounds)
    do
        println(s"\n== ROUND $round ==")
        assignMonradRankings(playerList)

        val possiblePairings = generatePairings(playerList)
        val legalPairings = filterIllegalPairings(possiblePairings, previousPairings)
        val chosenPairingList = selectOptimalPairing(legalPairings)

        val outcomes = scala.collection.mutable.Seq.fill(chosenPairingList.size)(None: Option[Outcome])
        while (!areAllOutcomesPresent(chosenPairingList, outcomes)) {

            println()
            chosenPairingList.zipWithIndex.foreach{
                case (pairing, index) => println(s"Pairing #${index+1}: $pairing")
            }

            print("\nEnter pairing index: ")
            try
                val index = scala.io.StdIn.readLine.toInt - 1
                chosenPairingList(index) match
                    case _: BracketPairing.Bye =>
                        println("[ERROR]: Cannot set the outcome of a bye")
                    case _: BracketPairing.Game =>
                        print("Please indicate the outcome (white/draw/black): ")
                        val outcome = scala.io.StdIn.readLine
                        outcome match
                            case "white" =>
                                outcomes(index) = Some(Outcome.White)
                            case "draw" =>
                                outcomes(index) = Some(Outcome.Draw)
                            case "black" =>
                                outcomes(index) = Some(Outcome.Black)
                            case _ =>
                                println("[ERROR]: Outcome unrecognized. Please enter \"white\", \"draw\", or \"black\".")
            catch
                case _: java.lang.NumberFormatException =>
                    println("[ERROR]: Please enter a valid integer index.")
                case _: java.lang.IndexOutOfBoundsException =>
                    println("[ERROR]: Please enter an index that is in bounds.")
        }

        previousPairings ++= chosenPairingList.toSet
        updateColorCounts(chosenPairingList)
        updateByeResult(chosenPairingList)
        updateScoresElosGamesPlayed(chosenPairingList, outcomes)
        updateDatabases(round, playerList, chosenPairingList, outcomes)

        val standings = playerList.groupBy(_.doubleScore).toSeq.sortBy(-_._1).map(_._2).foldLeft(Seq.empty)(
            (acc: Seq[(Int, BracketPlayer)], value: Seq[BracketPlayer]) => {
                acc ++ value.map((acc.size+1, _))
            })
        println(s"\n== CURRENT STANDINGS ==")
        standings.foreach{
            case (rank, player) => println(s"($rank) - $player")
        }
        print("\nPress [enter] to continue...")
        scala.io.StdIn.readLine
}

def generatePairings(playerList: Seq[BracketPlayer]): Seq[Seq[BracketPairing]] = {
    if (playerList.size % 2 == 0)
        evenPairings(playerList)
    else
        playerList.flatMap(byePlayer =>
            val remainingPlayers = playerList.filter(_ != byePlayer)
            evenPairings(remainingPlayers).map(pairingList =>
                BracketPairing.Bye(byePlayer) +: pairingList
            )
        )
}

def evenPairings(playerList: Seq[BracketPlayer]): Seq[Seq[BracketPairing]] = {
    if (playerList.size == 2) {
        Seq(
            Seq(BracketPairing.Game(playerList(0), playerList(1))),
            Seq(BracketPairing.Game(playerList(1), playerList(0)))
        )
    } else {
        playerList.flatMap(playerOne =>
            val remainingOne = playerList.filter(_ != playerOne)
            remainingOne.flatMap(playerTwo =>
                val remainingTwo = remainingOne.filter(_ != playerTwo)
                evenPairings(remainingTwo).flatMap(pairingList =>
                    Seq(
                        BracketPairing.Game(playerOne, playerTwo) +: pairingList,
                        BracketPairing.Game(playerTwo, playerOne) +: pairingList,
                    )
                )
            )
        )
    }
}

def filterIllegalPairings(
    possible: Seq[Seq[BracketPairing]],
    previous: Set[BracketPairing]): Seq[Seq[BracketPairing]] = {

    possible
        .filter(pairingList =>
            val proposedBye = pairingList.filter{
                case _: BracketPairing.Bye => true
                case _: BracketPairing.Game => false
            }
            if (proposedBye.size == 0)
                true
            else
                !previous.contains(proposedBye(0))
        )
        .filter(pairingList =>
            val proposedGames = pairingList.filter{
                case _: BracketPairing.Game => true
                case _: BracketPairing.Bye => false
            }.map{
                case BracketPairing.Game(white, black) => Set(white, black)
                case _ => Set.empty[BracketPlayer]
            }.toSet
            
            val previousGames = previous.filter{
                case _: BracketPairing.Game => true
                case _: BracketPairing.Bye => false
            }.map{
                case BracketPairing.Game(white, black) => Set(white, black)
                case _ => Set.empty[BracketPlayer]
            }

            proposedGames.intersect(previousGames).size == 0
        )
        .filter(pairingList =>
            pairingList.forall{
                case BracketPairing.Bye(_) => true
                case BracketPairing.Game(white, black) =>
                    (white.numWhiteGames - white.numBlackGames < 2 &&
                    black.numBlackGames - black.numWhiteGames < 2)
            }
        )
}

def selectOptimalPairing(legalPairings: Seq[Seq[BracketPairing]]): Seq[BracketPairing] = {

    legalPairings.sortBy(pairingList =>
        val byeScore = {
            pairingList.map{
                case BracketPairing.Bye(p) => p.doubleScore
                case BracketPairing.Game(_, _) => 0
            }.sum
        }
        val monradScore = {
            pairingList.map{
                case BracketPairing.Bye(_) => 0
                case BracketPairing.Game(white, black) =>
                    val gap = (white.monradRanking - black.monradRanking).abs - 1
                    gap * gap
            }.sum
        }
        val colorScore = {
            pairingList.map{
                case BracketPairing.Bye(_) => 0
                case BracketPairing.Game(white, black) =>
                    (white.numWhiteGames - white.numBlackGames +
                    black.numBlackGames - black.numWhiteGames)
            }.sum
        }
        (byeScore, monradScore, colorScore)
    ).head
}

def updateColorCounts(chosenPairingList: Seq[BracketPairing]): Unit = {
    chosenPairingList.foreach{
        case BracketPairing.Bye(_) => {}
        case BracketPairing.Game(white, black) =>
            white.numWhiteGames += 1
            black.numBlackGames += 1
    }
}

def updateByeResult(chosenPairingList: Seq[BracketPairing]): Unit = {
    chosenPairingList.foreach{
        case BracketPairing.Game(_, _) => {}
        case BracketPairing.Bye(p) => p.doubleScore += 2
    }
}

def areAllOutcomesPresent(
    chosenPairingList: Seq[BracketPairing],
    outcomes: scala.collection.mutable.Seq[Option[Outcome]]): Boolean = {

    chosenPairingList.zip(outcomes).map{
        case (pairing, outcome) => pairing match
            case _: BracketPairing.Bye => true
            case _: BracketPairing.Game => outcome match
                case Some(_) => true
                case None => false
    }.forall(identity)
}

def updateScoresElosGamesPlayed(
    chosenPairingList: Seq[BracketPairing],
    outcomes: scala.collection.mutable.Seq[Option[Outcome]]): Unit = {

    chosenPairingList.zip(outcomes).map{
        case (pairing, outcome) => outcome match
            case None => {}
            case Some(Outcome.White) => pairing match
                case BracketPairing.Bye(_) => {}
                case BracketPairing.Game(white, black) =>
                    white.doubleScore += 2
                    white.player.elo = newElo(white.player, black.player, 2)
                    black.player.elo = newElo(black.player, white.player, 0)
                    white.player.gamesPlayed += 1
                    black.player.gamesPlayed += 1
            case Some(Outcome.Draw) => pairing match
                case BracketPairing.Bye(_) => {}
                case BracketPairing.Game(white, black) =>
                    white.doubleScore += 1
                    black.doubleScore += 1
                    white.player.elo = newElo(white.player, black.player, 1)
                    black.player.elo = newElo(black.player, white.player, 1)
                    white.player.gamesPlayed += 1
                    black.player.gamesPlayed += 1
            case Some(Outcome.Black) => pairing match
                case BracketPairing.Bye(_) => {}
                case BracketPairing.Game(white, black) =>
                    black.doubleScore += 2
                    white.player.elo = newElo(white.player, black.player, 0)
                    black.player.elo = newElo(black.player, white.player, 2)
                    white.player.gamesPlayed += 1
                    black.player.gamesPlayed += 1
    }
}

def updateDatabases(
    round: Int,
    playerList: Seq[BracketPlayer],
    chosenPairingList: Seq[BracketPairing],
    outcomes: scala.collection.mutable.Seq[Option[Outcome]]): Unit = {

    savePlayers(playerList.map(_.player))
    chosenPairingList.zip(outcomes).map{
        case (pairing, outcome) => pairing.match
            case BracketPairing.Bye(p) =>
                addResult(round, Pairing.Bye(p.player), None)
            case BracketPairing.Game(w, b) =>
                addResult(round, Pairing.Game(w.player, b.player), outcome)
    }
}