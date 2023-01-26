package chessOS

import scala.math.pow

def newElo(elo: Int, numGames: Int, opponentElo: Int, doubleScore: Int): Int = {
    val kFactor = if (numGames < 10) 100.0 else 50.0
    val exponent = (opponentElo - elo) / 400.0
    val expectedScore = 1.0 / (1.0 + pow(10.0, exponent))
    (elo + kFactor * (doubleScore / 2.0 - expectedScore)).toInt
}