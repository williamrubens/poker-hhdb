package writers

import java.io.{FileOutputStream, PrintWriter}

/**
 * Created by willrubens on 06/08/15.
 */
class CsvWriter(perFileLimit: Integer, filename: String, delimiter: String = ", ") extends TrainingDataWriter {

    var writer: PrintWriter = null
    var fileCounter: Long = 0;
    var perFileCounter: Long = 0;
    val header = "vpip, pfr, street, potOdds, hasRaisedPreFlop, preflopRaises, percentStackCommited, scaledStackSize, " +
                 "myAggressionCount, nOppAggressors, playerHasChecked, distanceToButton, nActivePlayers, gameIsOpen, aceCount, " +
                 "kingCount, queenCount, jackCount, nPairs, nTrips, nFours, maxSuitCount, call, fold, raise";

    def write(trainingData: List[NNTrainingDatum]) = {

      if (perFileCounter > perFileLimit) {

        writer.flush()
        writer.close()

        perFileCounter = 0
        fileCounter += 1

        writer = null
      }

      if (writer == null) {

        val path = "mydb/" + filename + fileCounter + ".csv"

        println(path)
        val outputStream = new FileOutputStream(path, false)
        writer = new PrintWriter(outputStream)
        writer.println(header)

      }

      try {
        trainingData.map(datum => {
          val csvLine = datum.input.mkString(delimiter) + delimiter + datum.output.productIterator.mkString(delimiter)
          writer.println(csvLine)
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
