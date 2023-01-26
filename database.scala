package chessOS

import java.io._
import java.time._

case class Player (
    name:        String,
    elo:         Int = 1500,
    gamesPlayed: Int = 0
)

enum Outcome:
    case White, Draw, Black

enum Pairing:
    case Game(white: Player, black: Player)
    case Bye(player: Player)

case class PairingResult (
    date:        LocalDate,
    roundNumber: Int,
    pairing:     Pairing,
    outcome:     Option[Outcome]
)

val playerRegistryFname = "./db/playerRegistry.csv"
val pairingResultsFname = "./db/pairingResults.csv"

def getPlayer(name: String, playerList: Seq[Player]): Player = {
    playerList.filter(p => p.name == name).head
}

def loadPlayers: Seq[Player] = {
    scala.io.Source.fromFile(playerRegistryFname).getLines.toSeq.map( line =>
        val fields = line.split(";")
        Player(fields(0), fields(1).toInt, fields(2).toInt)
    )
}

def loadResults: Seq[PairingResult] = {

    val playerList = loadPlayers

    scala.io.Source.fromFile(pairingResultsFname).getLines.toSeq.map( line =>
        val fields = line.split(";")
        val date = LocalDate.parse(fields(0))
        fields(4) match
            case "None" =>
                val player = getPlayer(fields(2), playerList)
                PairingResult(date, fields(1).toInt, Pairing.Bye(player), None)
            case "White" =>
                val white = getPlayer(fields(2), playerList)
                val black = getPlayer(fields(3), playerList)
                PairingResult(date, fields(1).toInt, Pairing.Game(white, black), Some(Outcome.White))
            case "Draw" => 
                val white = getPlayer(fields(2), playerList)
                val black = getPlayer(fields(3), playerList)
                PairingResult(date, fields(1).toInt, Pairing.Game(white, black), Some(Outcome.Draw))
            case "Black" =>
                val white = getPlayer(fields(2), playerList)
                val black = getPlayer(fields(3), playerList)
                PairingResult(date, fields(1).toInt, Pairing.Game(white, black), Some(Outcome.Black))
    )
}

def addResult(roundNumber: Int, pairing: Pairing, outcome: Option[Outcome]): Unit = {

    val resultList = loadResults
    val newResult = PairingResult(LocalDate.now, roundNumber, pairing, outcome)
    val newResultList = resultList :+ newResult

    val pairingResultsFile = new FileWriter(new File(pairingResultsFname))
    newResultList.foreach(r => pairingResultsFile.write(
        r.pairing match
            case Pairing.Game(white, black) =>
                s"${r.date};${r.roundNumber};${white.name};${black.name};${r.outcome.get}\n"
            case Pairing.Bye(player) =>
                s"${r.date};${r.roundNumber};${player.name};None;None\n"
    ))
    pairingResultsFile.close()
}

def addPlayer(name: String): Unit = {

    val playerList = loadPlayers
    if (playerList.map(_.name).contains(name))
        println(s"[ERROR]: Cannot add player \"$name\", name is already in use.")
    else if (name == "None")
        println("[ERROR]: Player name cannot be \"None\".")
    else if (name.contains(";"))
        println("[ERROR]: Player name cannot contain \";\".")
    else if (name.trim == "")
        println("[ERROR]: Player name cannot be empty.")

    else
        val newPlayer = Player(name)
        val newPlayerList = playerList :+ newPlayer

        val playerRegistryFile = new FileWriter(new File(playerRegistryFname))
        newPlayerList.foreach(p => playerRegistryFile.write(
            s"${p.name};${p.elo};${p.gamesPlayed}\n"
        ))
        playerRegistryFile.close()
        println(s"[SUCCESS]: Player \"$name\" has been added.")
}

@main def main = {

    addPlayer("Michael")
    addPlayer("Chany")
    addPlayer("Guido")

    val playerList = loadPlayers
    val michael = playerList(0)
    val chany = playerList(1)
    val guido = playerList(2)

    addResult(1, Pairing.Game(michael, chany), Some(Outcome.White))
    addResult(1, Pairing.Bye(guido), None)

    addResult(2, Pairing.Game(guido, michael), Some(Outcome.Draw))
    addResult(2, Pairing.Bye(chany), None)

    addResult(3, Pairing.Game(chany, guido), Some(Outcome.Black))
    addResult(3, Pairing.Bye(michael), None)

    val resultList = loadResults
    println(s"$resultList")
}