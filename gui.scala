package chessOS

@main def main = {
    val playerList = ('A' to 'I').map(c => Player(c.toString))
    val numRounds = 5
    runBracket(playerList, numRounds)
}