package chessOS

import java.io._
import java.time._

class Player (
    val name:        String,
    var elo:         Int = 1500,
    var gamesPlayed: Int = 0
) {
    override def hashCode: Int = name.hashCode

    override def equals(that: Any): Boolean = {
        that match
            case that: Player => name == that.name
            case _ => false
    }

    override def toString: String = {
        val suffix = if (gamesPlayed < 10) "?" else ""
        s"${name} (${elo}${suffix})"
    }
}

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

        savePlayers(newPlayerList)
        println(s"[SUCCESS]: Player \"$name\" has been added.")
}

def savePlayers(playerList: Seq[Player]): Unit = {
    val playerRegistryFile = new FileWriter(new File(playerRegistryFname))
    playerList.foreach(p => playerRegistryFile.write(
        s"${p.name};${p.elo};${p.gamesPlayed}\n"
    ))
    playerRegistryFile.close()
}