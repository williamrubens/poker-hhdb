package writers

import java.io.{FileWriter, PrintWriter}

/**
 * Created by willrubens on 06/08/15.
 */
class CaseClassWriter(perFileLimit: Integer, filename: String,  header: String, delimiter: String = "\t ") {

   var writer: PrintWriter = null
   var fileCounter: Long = 0;
   var perFileCounter: Long = 0;

   def writeCaseClasses(cases:List[Product]) = {

     if(perFileCounter > perFileLimit) {
       perFileCounter = 0
       fileCounter += 1
       writer.flush()
       writer.close()
       writer = null
     }

     if(writer == null) {

       val actionFileName = "mydb/" + filename + fileCounter + ".csv"

       println(actionFileName)

       writer = new PrintWriter(new FileWriter(actionFileName))

       writer.println(header)
     }

     try{
       cases.map(_.productIterator.mkString(delimiter)) foreach (writer.println(_))
       writer.flush()
       perFileCounter += 1

     }

   }

   def close(): Unit = {

     if(writer != null) {
       writer.flush()
       writer.close()
     }

   }



 }
