package chessOS

import scala.math.pow

def newElo(elo: Int, gamesPlayed: Int, opponentElo: Int, doubleScore: Int): Int = {
    val kFactor = if (gamesPlayed < 10) 98.0 else 49.0
    val exponent = (opponentElo - elo) / 400.0
    val expectedScore = 1.0 / (1.0 + pow(10.0, exponent))
    (elo + kFactor * (doubleScore / 2.0 - expectedScore)).toInt
}