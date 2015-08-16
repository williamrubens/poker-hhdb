package adapters

import java.io._

import pokerai.hhex.helper.HandHelper
import pokerai.hhex.internal.HandManagerNIO
import pokerai.hhex.{Action, Consts, PokerHand}
import writers._

import scala.collection.mutable

/**
 * Created by willrubens on 05/05/15.
 */


object NNAdapter {

  def main(args: Array[String]) {

    var rootfolder: String = "/Users/willrubens/dev/poker-hhdb/hhdb_sample/"
    if (args.length > 0) rootfolder = args(0)
    if (!rootfolder.endsWith("/")) rootfolder += "/"
    Consts.rootFolder = rootfolder

//    val trainingDataWriter = new FannDataWriter(100000, "fann_data")
    val trainingDataWriter = new CsvWriter(100000, "csv_data")

    val nnAdapter = new NNAdapter(trainingDataWriter)


    // ---
    val dir: File = new File(rootfolder)
    val all: Array[String] = dir.list()
    // Scan for all files from site PS, Holde NL, 9-seats, and agregate stats for all found DBs

    for (i <- 0 until all.length) {
      if (all(i).startsWith("pokerai.org.sample" + Consts.SITE_PS + "_" + Consts.HOLDEM_FL + "_6") && all(i).endsWith(".hhex")) {

        nnAdapter.adapt(rootfolder, all(i))
      }
    }
  }
}





class NNAdapter(traingDataWriter: TrainingDataWriter) {


  var playerInfos = new mutable.HashMap[Int, PlayerInfo]

  val perFileLimit = 100000

  def updatePlayerInfos(h: PokerHand) = {

    val preflopActions = h.aactionsPreflop()

    for (seat <- 0 until h.getNumberOfSeats) {
      if (h.getStacks()(seat) > 0) {
        val playerId = h.getPlayerIDs()(seat)
        var playerInfo = playerInfos.getOrElseUpdate(playerId, new PlayerInfo(0, 0, 0, 0, 0, 0))

        if (playerId == 46118) {
          val a = playerId
        }

        playerInfo.handsPlayed += 1
        playerInfo.totalWinnings += h.getMoneyMade(seat.toByte)

        val winningActions = preflopActions.filter(_.getAction == Consts.ACTION_WON)

        playerInfo.totalWinningActions = winningActions.map(_.getAmount).sum

        val preflopPlayerActions = preflopActions.filter(_.getPlayerSeatId == seat)
        val vpipActions = preflopPlayerActions.filter(Consts.isVPIPAction(_))
        if (!preflopPlayerActions.isEmpty) {
          playerInfo.preflopPlayCount += 1
        }
        if (!vpipActions.isEmpty) {
          playerInfo.vpipCount += 1
          if (!vpipActions.filter(action => action.getAction == Consts.ACTION_BET || action.getAction == Consts.ACTION_RAISE).isEmpty) {
            playerInfo.pfrCount += 1
          }
        }

      }

    }

  }


  def buildPlayerStats() = {

    val players = playerInfos.map { case (playerId: Int, playerInfo: PlayerInfo) => {
      val vpip = playerInfo.vpipCount.toDouble / playerInfo.preflopPlayCount.toDouble
      val pfr = playerInfo.pfrCount.toDouble / playerInfo.preflopPlayCount.toDouble
      Player(playerId, playerInfo.handsPlayed, playerInfo.totalWinnings, vpip, pfr)
    }
    }.toList


    val playerStatsWriter = new CaseClassWriter(perFileLimit, "players", "PlayerId\t handsPlayed\t totalWinings\t totalWinningActions\tvpip\t pfr")

    playerStatsWriter.writeCaseClasses(players)

    playerStatsWriter.close

  }


  def buildGamesFile(handManager: HandManagerNIO): Unit = {

    val gamesWriter = new CaseClassWriter(perFileLimit, "games", "gameId\t gameType\t side\t seats\t date\t tableId\t button\t stacks\t playerIds\t smallBlind\t bigBlind\t communityCards\t playerCards")

    while (handManager.hasMoreHands) {
      val hand: PokerHand = handManager.nextPokerHand

      val playerCardsBuilder = List.newBuilder[String]
      for (i <- 0 until hand.getNumberOfSeats) {
        playerCardsBuilder += hand.getCards(i.toByte)
      }

      val game = Game(hand.getGameID, hand.getGameType, hand.getSite, hand.getNumberOfSeats, hand.getDate, hand.getTableID,
        hand.getButton, hand.getStacks.toList, hand.getPlayerIDs.toList, hand.smallBlind(), hand.bigBlind(),
        hand.getCommunityCards, playerCardsBuilder.result())


      updatePlayerInfos(hand)

      gamesWriter.writeCaseClasses(List(game))

    }

    gamesWriter.close
  }


  def buildNNTrainingData(hand: PokerHand, street: Byte, actions: Array[Action], potMan: PotManager) = {

    val preflopRaiseSeats = hand.aactionsPreflop().filter(Consts.isAggressiveAction(_)).map(_.getPlayerSeatId);

    var gameIsOpen = 1

    var boardCards = Array[Byte]()

    if(street == Consts.STREET_FLOP) {
      boardCards = hand.getCommunityCardsBA.take(3)
    } else if (street == Consts.STREET_TURN) {
      boardCards = hand.getCommunityCardsBA.take(4)
    } else if (street == Consts.STREET_RIVER) {
      boardCards = hand.getCommunityCardsBA.take(5)
    }


    val aggressiveActions = mutable.HashMap[Int, Int]()
    val playerChecks = mutable.MutableList[Int]()

    (0 until actions.size) flatMap {
      i => {
        val action = actions(i)

        val seat = action.getPlayerSeatId
        val trainDataOption = Consts.isVoluntaryGameAction(action) match {
          case false => None
          case true => {

            // features
            val playerId = hand.getPlayerIDs()(seat)
            val handHelper = new HandHelper()
            handHelper.setHand(hand, playerId)
            val playerInfo = playerInfos(playerId)
            val vpip = playerInfo.vpipCount.toDouble / playerInfo.preflopPlayCount.toDouble
            val pfr = playerInfo.pfrCount.toDouble / playerInfo.preflopPlayCount.toDouble

            val hasRaisedPreFlop = preflopRaiseSeats.contains(seat) match {
              case true => 1
              case false => 0
            }

            val percentStackCommited = potMan.playerPot(seat).toDouble / hand.getStacks()(seat).toDouble

            if(Consts.isAggressiveAction(action)) {
              var aggressiveActionCount = aggressiveActions.getOrElse(seat, 0)
              aggressiveActionCount += 1
              aggressiveActions.put(seat, aggressiveActionCount)

            }

            if(action.getAction == Consts.ACTION_CHECK) {
              playerChecks += seat

            }

            val myAggressionCount = aggressiveActions.getOrElse(seat, 0)
            val nOppAggressors = aggressiveActions.count( _._1 != seat )
            val playerHasChecked = playerChecks count ( _ == seat)
            val distanceToButton = handHelper.getDistanceToButton(true)
            val nActivePlayers = hand.getNumberOfActivePlayers(street)


            val aceCount = boardCards count (Consts.getCardRank(_) == Consts.RANK_ACE)
            val kingCount = boardCards count (Consts.getCardRank(_) == Consts.RANK_KING)
            val queenCount = boardCards count (Consts.getCardRank(_) == Consts.RANK_QUEEN)
            val jackCount = boardCards count (Consts.getCardRank(_) == Consts.RANK_JACK)
            val ranks = boardCards map (Consts.getCardRank(_))
            val rankCounts = ranks map(x => (x, ranks.count(_ == x))) distinct
            val nPairs = rankCounts count (_._2 == 2)
            val nTrips = rankCounts count (_._2 == 3)
            val nFours = rankCounts count (_._2 == 4)
            val suits = boardCards map (Consts.getCardSuit(_))
            val suitCounts = suits map(x =>  suits.count(_ == x)) distinct
            var maxSuitCount:Int = 0
            if(suitCounts.size > 0) {
              maxSuitCount = suitCounts max
            }
            val scaledStackSize = math.tanh(hand.getStacks()(seat) / 1000)

            // outputs
            val checkOrCall = action.getAction match {
              case Consts.ACTION_CHECK => 1
              case Consts.ACTION_CALL => 1
              case _ => 0
            }

            val fold = action.getAction match {
              case Consts.ACTION_FOLD => 1
              case _ => 0
            }

            val betOrRaise = action.getAction match {
              case Consts.ACTION_BET => 1
              case Consts.ACTION_RAISE => 1
              case _ => 0
            }


            val trainData = NNTrainingDatum(List(
              vpip,
              pfr,
              street,
              potMan.potOdds(seat),
              hasRaisedPreFlop,
              preflopRaiseSeats.size,
              percentStackCommited,
              scaledStackSize,
              myAggressionCount,
              nOppAggressors,
              playerHasChecked,
              distanceToButton,
              nActivePlayers,
              gameIsOpen,
              aceCount,
              kingCount,
              queenCount,
              jackCount,
              nPairs,
              nTrips,
              nFours,
              maxSuitCount), FannOutput(checkOrCall, fold, betOrRaise))
            Some(trainData)
          }

        }
        potMan.addPlayerPot(seat, action.getAmount)
        if(Consts.isAggressiveAction(action)) {
          gameIsOpen = 0
        }
        gameIsOpen
        trainDataOption
      }
    }
  }


  def buildNNTrainingSet(handManager: HandManagerNIO): Unit = {

    while (handManager.hasMoreHands) {

      val hand: PokerHand = handManager.nextPokerHand
      val nnInputsBuilder = List.newBuilder[NNTrainingDatum]

      val potMan = new PotManager

      if (hand.aactionsPreflop() != null) {
        val actions = hand.aactionsPreflop()
        nnInputsBuilder ++= buildNNTrainingData(hand, Consts.STREET_PREFLOP, actions, potMan)
      }
      if (hand.aactionsFlop() != null) {
        val actions = hand.aactionsFlop()
        nnInputsBuilder ++= buildNNTrainingData(hand, Consts.STREET_FLOP, actions, potMan)
      }
      if (hand.aactionsTurn() != null) {
        val actions = hand.aactionsTurn()
        nnInputsBuilder ++= buildNNTrainingData(hand, Consts.STREET_TURN, actions, potMan)
      }
      if (hand.aactionsRiver() != null) {
        val actions = hand.aactionsRiver()
        nnInputsBuilder ++= buildNNTrainingData(hand, Consts.STREET_RIVER, actions, potMan)
      }
      if (hand.aactionsShowdown() != null) {
        val actions = hand.aactionsShowdown()
        nnInputsBuilder ++= buildNNTrainingData(hand, Consts.STREET_SHOWDOWN, actions, potMan)
      }

      val nnInputs = nnInputsBuilder.result

      traingDataWriter.write(nnInputs)

    }

    traingDataWriter.close()

  }

  def getHandsDB(root: String, fullname: String) = {
    val hm: HandManagerNIO = new HandManagerNIO
    hm.init(root, fullname)
    hm.reset
    hm
  }

  def adapt(root: String, fullname: String) = {

    try {

      val handsDB1 = getHandsDB(root, fullname)

      buildGamesFile(handsDB1)

      buildPlayerStats()

      handsDB1.closedb()

      val handsDB2 = getHandsDB(root, fullname)

      buildNNTrainingSet(handsDB2)

      handsDB2.closedb()


    } catch {
      case e: Exception => {
        e.printStackTrace
        System.out.println(e.getMessage)
      }
    }
  }

}
