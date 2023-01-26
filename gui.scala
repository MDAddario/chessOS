package chessOS

@main def main = {

    // register these players into the system
    val playersToRegister = Seq("Michael", "Chany", "Guido", "Raffi", "Ian")
    playersToRegister.foreach(addPlayer(_))

    // these players wish to participate in the bracket
    val allPlayers = loadPlayers
    val playerList = Seq("Michael", "Chany", "Guido", "Raffi", "Ian").map(getPlayer(_, allPlayers))
    val numRounds = 3
    runBracket(playerList, numRounds)
}