import java.io.{File, FileWriter, PrintWriter}
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
import scala.io.Source

//java -jar /path/to/file.jar Main

object SPFinal {
  def main(args: Array[String]): Unit = {

    val DataGen = args(0)
    val Segments = args(1)
    val Rules = args(2)
    val Output = args(3)
    val Checkpoint = args(4)

    for (arg <- args){
      println(arg)
    }

    val twitterSet = scala.collection.mutable.Set[(String)]()
    val whatsappSet = scala.collection.mutable.Set[(String)]()
    val youtubeSet = scala.collection.mutable.Set[(String)]()
    val soundcloudSet = scala.collection.mutable.Set[(String)]()
    val instagramSet = scala.collection.mutable.Set[(String)]()
    val snapchatSet = scala.collection.mutable.Set[(String)]()
    val uberSet = scala.collection.mutable.Set[(String)]()
    val anghamiSet = scala.collection.mutable.Set[(String)]()

    //loading segment file as a list
    val segment = Source.fromFile(s"${Segments}").getLines.toList
    //println(segment)

    //loading rules file
    val rules = Source.fromFile(s"${Rules}").getLines().map(_.split(",")).toArray

    val twitter = rules(0)(0).toInt
    val whatsapp = rules(1)(0).toInt
    val youtube = rules(2)(0).toInt
    val soundcloud = rules(3)(0).toInt
    val instagram = rules(4)(0).toInt
    val snapchat = rules(5)(0).toInt
    val uber = rules(6)(0).toInt
    val anghami = rules(7)(0).toInt

    //function to get all files in directory in a LIST
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }

    var myFilesList = new ListBuffer[String]()

    //Checkpoint file
    var filesAlreadyReadSet = Source.fromFile(s"${Checkpoint}").getLines.toSet


    while (true) {


      val files = getListOfFiles(s"${DataGen}")

      for (file <- files) {

        breakable {
          //we're checking four things, if any condition is true we'll continue with the next iteration in the loop
          //(1) if the file name contains csv
          //(2) if the file list contains the file name
          //(3) if the file is empty
          //(4) if the checkpoint list of files (kinda like a history) contains the file
          if (!file.getName.contains(".csv") || myFilesList.contains(file.getName) || file.length() == 0 || filesAlreadyReadSet.contains(file.getName)) {
            break
          }

          myFilesList += file.getName


          //println(myFilesList)
          println("File Name " + file.getName)

          var read_file = io.Source.fromFile(file)
          //println(file)
          for (line <- read_file.getLines) {
            val cols = line.split(",")

            breakable {
              //Magic begins here
              if (!segment.contains(cols(0))) {
                println("This customer is not targeted")
                println(s"${cols(0)}" + " inside if")
                break
              }
              println(s"${cols(0)}")

              println(cols(1).toInt)


              cols(1).toInt match {
                //Here, we're checking three things based on the service ID: customer session consumption, lower and upper time bound
                case `twitter` => if (cols(2).toInt >= rules(0)(4).toInt && cols(3).substring(8, 10).toInt >= rules(0)(2).toInt && cols(3).substring(8, 10).toInt <= rules(0)(3).toInt) {
                  twitterSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in twitter case")
                  var twitterDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(0)(1)}"+".txt").getLines.toSet
                  println(twitterDuplicateCheckSet)
                  val twitterFw = new FileWriter(s"${Output}"+s"${rules(0)(1)}"+".txt", true)
                  val twitterPw = new PrintWriter(twitterFw)
                  for (line <- twitterSet) {
                    println(line)
                    println(!twitterDuplicateCheckSet.contains(line))
                    if(!twitterDuplicateCheckSet.contains(line)){
                      twitterPw.write(line+"\n")
                    }
                  }
                  twitterPw.close
                }

                case `whatsapp` => if (cols(2).toInt >= rules(1)(4).toInt && cols(3).substring(8, 10).toInt >= rules(1)(2).toInt && cols(3).substring(8, 10).toInt <= rules(1)(3).toInt) {
                  whatsappSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in whatsapp case")
                  var whatsappDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(1)(1)}"+".txt").getLines.toSet
                  println(whatsappDuplicateCheckSet)
                  val whatsappFw = new FileWriter(s"${Output}"+s"${rules(1)(1)}"+".txt", true)
                  val whatsappPw = new PrintWriter(whatsappFw)
                  for (line <- whatsappSet) {
                    println(line)
                    println(!whatsappDuplicateCheckSet.contains(line))
                    if(!whatsappDuplicateCheckSet.contains(line)){
                      whatsappPw.write(line+"\n")
                    }
                  }
                  whatsappPw.close
                }

                case `youtube` => if (cols(2).toInt >= rules(2)(4).toInt && cols(3).substring(8, 10).toInt >= rules(2)(2).toInt && cols(3).substring(8, 10).toInt <= rules(2)(3).toInt) {
                  youtubeSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in youtube case")
                  var youtubeDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(2)(1)}"+".txt").getLines.toSet
                  println(youtubeDuplicateCheckSet)
                  val youtubeFw = new FileWriter(s"${Output}"+s"${rules(2)(1)}"+".txt", true)
                  val youtubePw = new PrintWriter(youtubeFw)
                  for (line <- youtubeSet) {
                    println(line)
                    println(!youtubeDuplicateCheckSet.contains(line))
                    if(!youtubeDuplicateCheckSet.contains(line)){
                      youtubePw.write(line+"\n")
                    }
                  }
                  youtubePw.close
                }

                case `soundcloud` => if (cols(2).toInt >= rules(3)(4).toInt && cols(3).substring(8, 10).toInt >= rules(3)(2).toInt && cols(3).substring(8, 10).toInt <= rules(3)(3).toInt) {
                  soundcloudSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in soundcloud case")
                  var soundcloudDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(3)(1)}"+".txt").getLines.toSet
                  println(soundcloudDuplicateCheckSet)
                  val soundcloudFw = new FileWriter(s"${Output}"+s"${rules(3)(1)}"+".txt", true)
                  val soundcloudPw = new PrintWriter(soundcloudFw)
                  for (line <- soundcloudSet) {
                    println(line)
                    println(!soundcloudDuplicateCheckSet.contains(line))
                    if(!soundcloudDuplicateCheckSet.contains(line)){
                      soundcloudPw.write(line+"\n")
                    }
                  }
                  soundcloudPw.close
                }

                case `instagram` => if (cols(2).toInt >= rules(4)(4).toInt && cols(3).substring(8, 10).toInt >= rules(4)(2).toInt && cols(3).substring(8, 10).toInt <= rules(4)(3).toInt) {
                  instagramSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  var instagramDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(4)(1)}"+".txt").getLines.toSet
                  println(instagramDuplicateCheckSet)
                  val instagramFw = new FileWriter(s"${Output}"+s"${rules(4)(1)}"+".txt", true)
                  val instagramPw = new PrintWriter(instagramFw)
                  for (line <- instagramSet) {
                    println(line)
                    println(!instagramDuplicateCheckSet.contains(line))
                    if(!instagramDuplicateCheckSet.contains(line)){
                      instagramPw.write(line+"\n")
                    }
                  }
                  instagramPw.close
                }

                case `snapchat` => if (cols(2).toInt >= rules(5)(4).toInt && cols(3).substring(8, 10).toInt >= rules(5)(2).toInt && cols(3).substring(8, 10).toInt <= rules(5)(3).toInt) {
                  snapchatSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in snapchat case")
                  var snapchatDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(5)(1)}"+".txt").getLines.toSet
                  println(snapchatDuplicateCheckSet)
                  val snapchatFw = new FileWriter(s"${Output}"+s"${rules(5)(1)}"+".txt", true)
                  val snapchatPw = new PrintWriter(snapchatFw)
                  for (line <- snapchatSet) {
                    println(line)
                    if(!snapchatDuplicateCheckSet.contains(line)){
                      snapchatPw.write(line+"\n")
                    }
                  }
                  snapchatPw.close
                }

                case `uber` => if (cols(2).toInt >= rules(6)(4).toInt && cols(3).substring(8, 10).toInt >= rules(6)(2).toInt && cols(3).substring(8, 10).toInt <= rules(6)(3).toInt) {
                  uberSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in uber case")
                  var uberDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(6)(1)}"+".txt").getLines.toSet
                  println(uberDuplicateCheckSet)
                  val uberFw = new FileWriter(s"${Output}"+s"${rules(6)(1)}"+".txt", true)
                  val uberPw = new PrintWriter(uberFw)
                  for (line <- uberSet) {
                    println(line)
                    if(!uberDuplicateCheckSet.contains(line)){
                      uberPw.write(line+"\n")
                    }
                  }
                  uberPw.close
                }

                case `anghami` => if (cols(2).toInt >= rules(7)(4).toInt && cols(3).substring(8, 10).toInt >= rules(7)(2).toInt && cols(3).substring(8, 10).toInt <= rules(7)(3).toInt) {
                  anghamiSet += (cols(0).mkString + "," + cols(1).mkString + "," + cols(2).mkString + "," + cols(3).mkString.substring(0, 8))
                  println("in anghami case")
                  var anghamiDuplicateCheckSet = Source.fromFile(s"${Output}"+s"${rules(7)(1)}"+".txt").getLines.toSet
                  println(anghamiDuplicateCheckSet)
                  val anghamiFw = new FileWriter(s"${Output}"+s"${rules(7)(1)}"+".txt", true)
                  val anghamiPw = new PrintWriter(anghamiFw)
                  for (line <- anghamiSet) {
                    println(line)
                    if(!anghamiDuplicateCheckSet.contains(line)){
                      anghamiPw.write(line+"\n")
                    }
                  }
                  anghamiPw.close
                }

                case _ => println("other Application is used ")

              }
            }

          }
          filesAlreadyReadSet += file.getName
          //In the next (4) lines, we're writing the contents of the file list
          //            val checkpointFw = new FileWriter("E:\\ITI\\Courses\\Scala\\Project\\Checkpoint\\FilesRead.txt",true)
          val checkpointPw = new PrintWriter(s"${Checkpoint}")
          for (file <- filesAlreadyReadSet) {
            checkpointPw.write(file + "\n")
          }
          checkpointPw.close

          //println(cols.getClass)
          //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")

          read_file.close

        }
      }
    }
  }
}

