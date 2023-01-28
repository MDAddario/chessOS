package chessOS

import scala.util.Random
import scala.collection.mutable.{Seq => MutSeq}

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
    
    def canPlayWhite = (numWhiteGames - numBlackGames) < 2
    def canPlayBlack = (numBlackGames - numWhiteGames) < 2

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
    println("[DEBUG]: The random seeds are being fucked with. This cannot go in prod.")
    // Random.shuffle(playerList).zipWithIndex.map{
    //     case (p, i) => p.randomSeed = i
    // }
    playerList.zipWithIndex.map{
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

    val resumePausedBracket = false
    var previousPairings = if (resumePausedBracket) resumeBracket(playerList) else Set.empty[BracketPairing]

    for
        round <- (1 to numRounds)
    do
        println(s"\n== ROUND $round ==")
        assignMonradRankings(playerList)

        val chosenPairingList = 
            if (round == 1)
                generateFirstRoundPairings(playerList)
            else
                val legalPairings = makeLegalPairings(playerList, previousPairings)
                println(s"[DEBUG]: Number of legal pairings: ${legalPairings.size}")
                val uniques = legalPairings.map(_.toSet).toSet
                println(s"[DEBUG]: Number of non-duplicates: ${uniques.size}")
                selectOptimalPairing(legalPairings)
        val outcomes = askUserForOutcomes(chosenPairingList)

        previousPairings ++= chosenPairingList.toSet
        updateColorCounts(chosenPairingList)
        updateByeResult(chosenPairingList)
        updateScoresElosGamesPlayed(chosenPairingList, outcomes)
        updateDatabases(round, playerList, chosenPairingList, outcomes)

        printStandings(playerList)
        print("\nPress [enter] to continue...")
        scala.io.StdIn.readLine
}

def generateFirstRoundPairings(playerList: Seq[BracketPlayer]): Seq[BracketPairing] =
    _gfrp(playerList.sortBy(_.monradRanking))

def _gfrp(sortedList: Seq[BracketPlayer]): Seq[BracketPairing] = {

    sortedList.size match
        case 0 =>
            Seq.empty[BracketPairing]
        case 1 =>
            Seq(BracketPairing.Bye(sortedList(0)))
        case _ =>
            BracketPairing.Game(sortedList(0), sortedList(1)) +: _gfrp(sortedList.drop(2))
}

enum PotentialPairing:
    case ColorsAmbiguous(p1: BracketPlayer, p2: BracketPlayer)
    case ColorsFixed(w: BracketPlayer, b: BracketPlayer)
    case Bye(p: BracketPlayer)

def makeLegalPairings(playerList: Seq[BracketPlayer], previousPairings: Set[BracketPairing]):
    Seq[Seq[PotentialPairing]] = {
    
    _mlp(playerList, previousPairings).filter{
        case Some(_) => true
        case None => false
    }.map(_.get)
}
def _mlp(playerList: Seq[BracketPlayer], previousPairings: Set[BracketPairing]): 
    Seq[Option[Seq[PotentialPairing]]] = {

    playerList.size match
        case 0 =>
            Seq(Some(Seq.empty))
        case 1 =>
            val p = playerList(0)
            if (previousPairings.contains(BracketPairing.Bye(p)))
                Seq(None)
            else
                Seq(Some(Seq(PotentialPairing.Bye(p))))
        case _ =>
            playerList.flatMap(p1 =>
                val oneRemoved = playerList.filter(_ != p1)
                oneRemoved.flatMap(p2 =>
                    val twoRemoved = oneRemoved.filter(_ != p2)

                    if (previousPairings.contains(BracketPairing.Game(p1,p2)) || 
                        previousPairings.contains(BracketPairing.Game(p2,p1)))
                        Seq(None)
                    else
                        _mlp(twoRemoved, previousPairings).flatMap{
                            case None =>
                                Seq(None)
                            case Some(pairingList) =>

                                val forward = p1.canPlayWhite && p2.canPlayBlack
                                val reverse = p2.canPlayWhite && p1.canPlayBlack

                                (forward, reverse) match
                                    case (false, false) =>
                                        Seq(None)
                                    case (true, false) =>
                                        Seq(Some(PotentialPairing.ColorsFixed(p1,p2) +: pairingList))
                                    case (false, true) =>
                                        Seq(Some(PotentialPairing.ColorsFixed(p2,p1) +: pairingList))
                                    case (true, true) =>
                                        Seq(Some(PotentialPairing.ColorsAmbiguous(p1,p2) +: pairingList))
                        }
                )
            )
}

def optimalColors(potensh: PotentialPairing): BracketPairing = {
    potensh match
        case PotentialPairing.Bye(p) =>
            BracketPairing.Bye(p)
        case PotentialPairing.ColorsFixed(w, b) =>
            BracketPairing.Game(w, b)
        case PotentialPairing.ColorsAmbiguous(p1, p2) =>
            val p1IsWhite = (p1.numBlackGames - p1.numWhiteGames) > (p2.numBlackGames - p2.numWhiteGames)
            if (p1IsWhite)
                BracketPairing.Game(p1, p2)
            else
                BracketPairing.Game(p2, p1)
}

def selectOptimalPairing(legalPairings: Seq[Seq[PotentialPairing]]): Seq[BracketPairing] = {

    legalPairings.map(pl => pl.map(optimalColors(_))).sortBy(pairingList =>
        val byeScore = {
            pairingList.map{
                case BracketPairing.Bye(p) => p.doubleScore
                case BracketPairing.Game(_, _) => 0
            }.sum
        }
        val monradScore = {
            pairingList.map{
                case BracketPairing.Bye(_) => 0
                case BracketPairing.Game(w, b) =>
                    val gap = (w.monradRanking - b.monradRanking).abs - 1
                    gap * gap
            }.sum
        }
        val colorScore = {
            pairingList.map{
                case BracketPairing.Bye(_) => 0
                case BracketPairing.Game(w, b) =>
                    w.numWhiteGames - w.numBlackGames + b.numBlackGames - b.numWhiteGames
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
                    val eloW = newElo(white.player, black.player, 2)
                    val eloB = newElo(black.player, white.player, 0)
                    white.player.elo = eloW
                    black.player.elo = eloB
                    white.player.gamesPlayed += 1
                    black.player.gamesPlayed += 1
            case Some(Outcome.Draw) => pairing match
                case BracketPairing.Bye(_) => {}
                case BracketPairing.Game(white, black) =>
                    white.doubleScore += 1
                    black.doubleScore += 1
                    val eloW = newElo(white.player, black.player, 1)
                    val eloB = newElo(black.player, white.player, 1)
                    white.player.elo = eloW
                    black.player.elo = eloB
                    white.player.gamesPlayed += 1
                    black.player.gamesPlayed += 1
            case Some(Outcome.Black) => pairing match
                case BracketPairing.Bye(_) => {}
                case BracketPairing.Game(white, black) =>
                    black.doubleScore += 2
                    val eloW = newElo(white.player, black.player, 0)
                    val eloB = newElo(black.player, white.player, 2)
                    white.player.elo = eloW
                    black.player.elo = eloB
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

def resumeBracket(playerList: Seq[BracketPlayer]): Set[BracketPairing] = {
    val oldResults = loadResults

    oldResults.foreach{ pairingResult => 
    (pairingResult.outcome, pairingResult.pairing) match
        case (Some(Outcome.White), Pairing.Game(w,b)) =>
            playerList.filter(_.player == w).head.doubleScore += 2
        case (Some(Outcome.Draw), Pairing.Game(w,b)) => 
            playerList.filter(_.player == w).head.doubleScore += 1
            playerList.filter(_.player == b).head.doubleScore += 1
        case (Some(Outcome.Black), Pairing.Game(w,b)) => 
            playerList.filter(_.player == b).head.doubleScore += 2
        case (None, Pairing.Bye(p)) =>
            playerList.filter(_.player == p).head.doubleScore += 2
        case _ => {}
    }

    oldResults.toSet.map(_.pairing match
        case Pairing.Game(w, b) => BracketPairing.Game(BracketPlayer(w), BracketPlayer(b))
        case Pairing.Bye(p) => BracketPairing.Bye(BracketPlayer(p))
    )
}

def printStandings(playerList: Seq[BracketPlayer]): Unit = {
    val standings = playerList.groupBy(_.doubleScore).toSeq.sortBy(-_._1).map(_._2).foldLeft(Seq.empty)(
        (acc: Seq[(Int, BracketPlayer)], value: Seq[BracketPlayer]) => {
            acc ++ value.map((acc.size+1, _))
        })
    println(s"\n== CURRENT STANDINGS ==")
    standings.foreach{
        case (rank, player) => println(s"($rank) - $player")
    }
}

def askUserForOutcomes(chosenPairingList: Seq[BracketPairing]): MutSeq[Option[Outcome]] = {
    val outcomes = MutSeq.fill(chosenPairingList.size)(None: Option[Outcome])
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
    outcomes
}