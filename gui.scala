package chessOS

// add players to registry
// select which players will play in the tournament

@main def main = {
    val players = Seq("Michael", "Chany", "Guido")
    players.foreach(addPlayer(_))

    val numRounds = 3
    runBracket(loadPlayers, numRounds)
}