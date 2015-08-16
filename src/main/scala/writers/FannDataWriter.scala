package writers

import java.io.{FileOutputStream, FileWriter, PrintWriter}

/**
 * Created by willrubens on 24/07/15.
 */

//case class NNInput(vpip: Double, pfr: Double, street: Int, actionNumber: Int, ActionType: Int,  winnings: Long)

//// if you change here, don't forget to add to the frann writer
//case class FannInputVPSA(vpip: Double, pfr: Double, street: Int, actionNumber: Int)
//case class FannInputVPP(vpip: Double, pfr: Double, potOdds: Double);
//case class FannInputVP(vpip: Double, pfr: Double);
case class FannOutput(checkOrCall: Int, fold: Int, betOrRaise: Int)

//case class FannTrainingDatum(input: FannInput, output: FannOutput)
case class NNTrainingDatum(input: List[Any], output: FannOutput)


case class Game(gameId: Long, gameType: Int, site: Int, seats: Int, date: Long, tableId: Int,
                button: Int, stacks:List[Int], playerIds:List[Int], smallBlind: Int, bigBlind: Int,
                communityCards: String, playerCards: List[String])

//case class Action(gameId: Long, gameStage: Int, actionNum: Int, seat: Int,
//                  actionType: Int, amount: Int)

case class Player(playerId: Int, handsPlayed: Int, totalWinnings: Long, vpip: Double, pfr: Double)

class PlayerInfo(var handsPlayed: Int, var totalWinnings: Long, var totalWinningActions: Int, var preflopPlayCount: Int,  var vpipCount: Int,  var pfrCount: Int)



class FannDataWriter(perFileLimit: Integer, filename: String, delimiter: String = " ") extends TrainingDataWriter {

  var writer: PrintWriter = null
  var fileCounter: Long = 0;
  var perFileCounter: Long = 0;
  val header = s"$perFileLimit 4 3"

  def write(trainingData: List[NNTrainingDatum]) = {

    if (perFileCounter > perFileLimit) {

      writer.flush()
      writer.close()

      perFileCounter = 0
      fileCounter += 1

      writer = null
    }

    if (writer == null) {

      val path = "mydb/" + filename + fileCounter + ".txt"

      println(path)
      val outputStream = new FileOutputStream(path, false)
      writer = new PrintWriter(outputStream)
      writer.println(header)

    }

    try {
      trainingData.map(datum => {
        writer.println(datum.input.productIterator.mkString(delimiter))
        writer.println(datum.output.productIterator.mkString(delimiter))
      })

      writer.flush()
      perFileCounter += trainingData.size

    }

  }

  def close(): Unit = {

    if(writer != null) {
      writer.flush()
      writer.close()
    }

  }



}
