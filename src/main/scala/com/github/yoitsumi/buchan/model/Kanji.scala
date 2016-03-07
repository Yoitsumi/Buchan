package com.github.yoitsumi.buchan.model

/**
 * Created by Kamil on 06.03.2016.
 */
case class Kanji(char: Char, parts: Seq[Part], original: Option[Char]) {


  def isEquivalent(radical: Kanji): Boolean =
    this.char == radical.char || original.contains(radical.char) || radical.original.contains(char)

  def flatten: Seq[Kanji] =
    if(parts.isEmpty) Seq(this)
    else this +: parts.sortBy(_.position.ord).flatMap(_.kanji.flatten)

}

object Kanji {

  val KvgNamespace = "http://kanjivg.tagaini.net"

  def load(g: xml.Node): Kanji = {
    //    val char = (g \ s"@{$KvgNamespace}element").toString.charAt(0)
    val char = (for {
      attr <- g.attributes
      if attr.key == "element" || attr.key == "phon"
    } yield attr.value.headOption.getOrElse("?").toString.charAt(0)).headOption.getOrElse('?')
    val original = g.attribute(KvgNamespace, "original").map(_.mkString.charAt(0))
    val parts = for {
      subg <- g \ "g"
    } yield Part.load(subg)
    Kanji(char, parts, original)
  }

  def loadKanjiFile(svg: xml.Node): Kanji = {
    val g = for {
      g <- svg \ "g"
      id <- g \ "@id"
      if id.toString startsWith "kvg:StrokePaths_"
      ret <- g \ "g"
    } yield ret
    if (g.size != 1) throw new RuntimeException("Couldn't find apropriate <g> tag dor kanji")
    else load(g.head)
  }

}
