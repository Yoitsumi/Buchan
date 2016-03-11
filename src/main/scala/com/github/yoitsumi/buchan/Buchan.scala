package com.github.yoitsumi.buchan

import java.awt.im.InputMethodRequests
import java.io.{FileFilter, File}
import javafx.collections.{FXCollections, ObservableList}
import javafx.scene.input
import javax.xml.parsers.SAXParserFactory

import com.github.yoitsumi.buchan.model._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.xml.XML
import scalafx.application.{Platform, JFXApp}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.Observable
import scalafx.beans.property.ObjectProperty
import scalafx.beans.value.ObservableValue
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{ListCell, ListView, TextField}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{Priority, HBox, VBox}
import scalafx.Includes._

import scala.concurrent.ExecutionContext.Implicits.global
import scalafx.scene.text.Font
import scalafx.scene.web.WebView
import scalafx.stage.Popup

/**
 * Created by Kamil on 06.03.2016.
 */
object Buchan extends JFXApp {


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

  var allKanji: Seq[Kanji] = Seq()
  val allKanjiFuture = Future { for {
    file <- new File("./kanji/").listFiles((f: File) => (f.getName endsWith ".svg") && !(f.getName contains "-"))
    xml = try {
      xmlParser.load(file.toURI.toURL)
    } catch {
      case NonFatal(ex) => throw new RuntimeException(s"exception caught while parsing file $file", ex)
    }
  } yield Kanji.loadKanjiFile(xml) }
  for(x <- allKanjiFuture) {
    allKanji = x
    Platform.runLater{
      radicalInput.disable = false
      radicalInput.requestFocus()
    }
  }

  val globalFont = Font("Meiryo", 20)

  val radicalInput: TextField = new TextField {
    onAction = { e: ActionEvent =>
      if(text.value.length() > 0) {
        resultList.items.get().headOption.foreach(c => wordInput.text.value = wordInput.text.value + c)
        text = ""
      } else {
        println(s"searching for ${wordInput.text.value}")
        jishoWebView.getEngine.load(s"http://jisho.org/search/${wordInput.text.value}")
      }
    }
    onKeyPressed = { e: KeyEvent =>
      e.code match {
        case KeyCode.DOWN =>
          resultList.selectionModel.value.select(1)
          resultList.requestFocus()
        case KeyCode.UP =>
          resultList.selectionModel.value.select(0)
          resultList.requestFocus()
        case _ =>
      }
    }
    disable = true
    font = globalFont
  }

  val wordInput = new TextField {
    font = globalFont
    onAction = { _: ActionEvent =>
      println(s"searching for ${text.value}")
      jishoWebView.getEngine.load(s"http://jisho.org/search/${text.value}")
    }
  }
  val resultList: ListView[Char] = new ListView[Char] {
    items <== radicalInput.text.map { radicals =>
      FXCollections.observableArrayList(onDoSearch(radicals).map(_.char).toArray: _*)
    }
    cellFactory = { _ =>
      new ListCell[Char] {
        text <== item.map(_.toString)
        font = globalFont
        onKeyPressed = { e: KeyEvent =>
          if(e.code == KeyCode.ENTER) {
            resultList.items.get().headOption.foreach{c =>
              wordInput.text.value = wordInput.text.value + c
              radicalInput.text = ""
              radicalInput.requestFocus()
            }
          }
        }
      }
    }
  }
  val jishoWebView = new WebView {
//    minWidth = 100
    vgrow = Priority.ALWAYS
  }

  stage = new PrimaryStage {
    title = "Kanji Lookup"
    scene = new Scene {
      content = new HBox(10) {
        content = Seq(
          new VBox(10) {
            padding = Insets(10)
            content = Seq(
              radicalInput,
              wordInput,
              resultList
            )
          },
          jishoWebView
        )
      }
    }
    show()
  }

  implicit class ObservableUtils[V](val underlaying: ObservableValue[V, _]) {
    def map[W](f: V => W): ObservableValue[W, W] = {
      @inline def recalculate(): W = f(underlaying.value)

      val originalValue = recalculate()

      val prop = ObjectProperty[W](originalValue)

      var prevValue = originalValue
      def changeHandler = prop.synchronized {
        val newVal = recalculate()
        if (prevValue != newVal) {
          prop.value = recalculate()
          prevValue = newVal
        }
      }

      underlaying onChange changeHandler
      prop
    }
  }

  // TODO quick copy-paste, clean this up

  def onDoSearch(chars: String): Seq[Kanji] = {
//    val chars = radicalInput.text.value
    val kanji = chars.flatMap(char => allKanji.find(_.char == char))
    val results = search(kanji)
    kanji.foreach{k =>
      println(k.flatten.map(_.char).mkString(" "))
      println(k.parts)
    }
    if (kanji.isEmpty) {
      System.err.println(s"Can't find kanji for $chars")
    }
    results
  }

  def search(k: Seq[Kanji]) = {
    if(k.isEmpty) Seq.empty
    else exactRadicalSearch(k) ++: halfSearch(k) ++: fullSearch(k)
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

  def fullSearch(patterns: Seq[Kanji]): Seq[Kanji] = {
    def recur(parts: Seq[Kanji], currentPattern: Seq[Kanji], patterns: Seq[Kanji], currentDepth: Int = 0, depthSum: Int = 0): Option[Int] =
      (parts, currentPattern, patterns) match {
      case (_, _, Seq()) => Some(depthSum)
      case (_, Seq(), _) => None
      case (Seq(), _, _) => None
      case (part +: partsRest, patPart +: patRest, pattern +: patternsRest)
        if part == patPart =>
          recur(partsRest, patternsRest.headOption.map(_.flatten).getOrElse(Seq.empty), patternsRest, 0, depthSum + currentDepth)
      case (part +: partsRest, _ +: patRest, _) =>
          recur(partsRest, patRest, patterns, currentDepth + 1, depthSum)
    }
    (for {
      kanji <- allKanji
      depth <- recur(kanji.flatten, patterns.head.flatten, patterns)
    } yield (kanji, depth)).sortBy(_._2).map(_._1).distinct
  }

}
