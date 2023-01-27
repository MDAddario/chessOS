package chessOS

@main def main = {
    val playerList = loadPlayers
    val numRounds = 3
    runBracket(playerList, numRounds)
}