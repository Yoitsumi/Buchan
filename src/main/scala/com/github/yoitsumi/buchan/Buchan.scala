package com.github.yoitsumi.buchan

import java.io.{FileFilter, File}
import javax.xml.parsers.SAXParserFactory

import com.github.yoitsumi.buchan.model._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.xml.XML
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.TextField
import scalafx.scene.layout.VBox

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by Kamil on 06.03.2016.
 */
object Buchan extends JFXApp {

  val radicalInput = new TextField
  val wordInput = new TextField

  stage = new PrimaryStage {
    title = "Kanji Lookup"
    scene = new Scene {
      content = new VBox(10) {
        content = Seq(
          radicalInput,
          wordInput
        )
      }
    }
    show()
  }


  // TODO quick copy-paste, clean this up

  lazy val sax = {
    val s = SAXParserFactory.newInstance()
    s.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    s.newSAXParser()
  }

  lazy val xmlParser = XML.withSAXParser(sax)

  implicit def `File => Boolean 2 FileFilter`(f: File => Boolean): FileFilter =
    new FileFilter {
      override def accept(file: File): Boolean = f(file)
    }

  var allKanji: Seq[Kanji] = _
  val allKanjiFuture = Future { for {
    file <- new File("./kanji/").listFiles((f: File) => (f.getName endsWith ".svg") && !(f.getName contains "-"))
    xml = try{
      xmlParser.load(file.toURI.toURL)
    } catch {
      case NonFatal(ex) => throw new RuntimeException(s"exception caught while parsing file $file", ex)
    }
  } yield Kanji.loadKanjiFile(xml) }
  for(x <- allKanjiFuture) allKanji = x

  def onDoSearch() = {
    val chars = radicalInput.text.value
    val kanji = chars.flatMap(char => allKanji.find(_.char == char))
    search(kanji)
    kanji.foreach{k =>
      println(k.flatten.map(_.char).mkString(" "))
      println(k.parts)
    }
    if (kanji.isEmpty) {
      System.err.println(s"Can't find kanji for $chars")
    }
  }

  def search(k: Seq[Kanji]) = {

    val results = exactRadicalSearch(k) ++: halfSearch(k)

    // TODO display results
  }

  def exactRadicalSearch(radicals: Seq[Kanji]): Seq[Kanji] = {
    def recur(parts: Seq[Kanji], toFind: Seq[Kanji]): Boolean = (parts, toFind) match {
      case (_, Seq()) => true
      case (Seq(), _) => false
      case (part +: partRest, radical +: radicalsRest) =>
        if(part.isEquivalent(radical))
          recur(partRest, radicalsRest)
        else
          recur(partRest, toFind)
    }
    for {
      kanji <- allKanji
      if recur(kanji.flatten, radicals)
    } yield kanji
  }

  def halfSearch(patterns: Seq[Kanji]): Seq[Kanji] = {
    for {
      kanji <- allKanji
      if patterns.forall(_.parts.exists(part => kanji.parts.contains(part)))
    } yield kanji
  }

}
