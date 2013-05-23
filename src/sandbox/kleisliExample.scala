package sandbox

import scalaz._
import Kleisli._
import std.list._

import java.io.File
import scala.io.Source

// http://www.cakesolutions.net/teamblogs/2011/09/18/kleisli-arrows-ii/
// scalaz 6: >=> and <=<
// scalaz 7: >==> and <==<
object kleisliExample {

  def lines(dir: String): List[File] = new File(dir).listFiles.toList
  def lengths(f: File): List[Int] = Source.fromFile(f).getLines.map(_.size).toList

  def dirLengths = kleisli(lines) >==> lengths
  def homeLineLengths = kleisli(lines) >==> lengths <==< ((home: String) => List("D:/temp/" + home))
  def myLineLengths = (kleisli(lines) >==> lengths) =<< (List("D:/temp/00"))

  def recLengths: Kleisli[List, File, Int] = kleisli((f: File) => {
    if (f.isDirectory) recLengths =<< f.listFiles.toList
    else lengths(f)
  })
  def dirRecLengths = kleisli(lines) >==> recLengths
  def homeLineRecLengths = kleisli(lines) >==> recLengths <==< ((home: String) => List("D:/temp/" + home))
  def myLineRecLengths = (kleisli(lines) >==> recLengths) =<< (List("D:/temp/01"))

  
  def main(args: Array[String]) {
    println(dirLengths("D:/temp/00"))
    println(homeLineLengths("00"))
    println(myLineLengths)

    println(dirRecLengths("D:/temp/01"))
    println(homeLineRecLengths("01"))
    println(myLineRecLengths)

    
  }

}