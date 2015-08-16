package writers

import java.io.{FileOutputStream, PrintWriter}

/**
 * Created by willrubens on 06/08/15.
 */
trait TrainingDataWriter {

  def write(trainingData: List[NNTrainingDatum])

  def close(): Unit
}
