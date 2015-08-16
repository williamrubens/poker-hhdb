package adapters

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
 * Created by willrubens on 01/08/15.
 */
class PotManager{

  val playerPots = new TrieMap[Int, Int]


  def playerPot(seatId: Int) = {
    playerPots.getOrElse(seatId, 0)
  }

  def addPlayerPot(seatId: Int, amount: Int) = {
    val total = playerPots.getOrElse(seatId, 0) + amount

    playerPots.put(seatId, total)
  }

  def setPlayerPot(seatId: Int, amount: Int) = {
    playerPots.put(seatId, amount)
  }

  def potOdds(seatId: Int): Double = {
    // find highest bet

    if (playerPots.size == 0) {
      return 0.0
    }

    val bids = playerPots.values.map(_.toInt)

    val highestBid = bids.max

    val playerBid = playerPots.getOrElse(seatId, 0)

    val toCall = highestBid - playerBid

    val totalPot = bids.sum
    if((toCall.toDouble + totalPot.toDouble) == 0.0) {
      val pause = true
    }
    toCall.toDouble / (toCall.toDouble + totalPot.toDouble)
  }
}
