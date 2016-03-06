package com.github.yoitsumi.buchan.model

/**
 * Created by Kamil on 06.03.2016.
 */
case class Part(kanji: Kanji, position: Position) {
  def char: Char = kanji.char

  def isEquivalent(part: Part): Boolean =
    kanji == part.kanji || kanji.original.contains(part.char) || part.kanji.original.contains(char)

  def isEquivalent(b: Kanji): Boolean =
    kanji == b || kanji.original.contains(b.char) || b.original.contains(char)
}

object Part {

  def load(g: xml.Node): Part = {
    val posName = (g \ s"@{${Kanji.KvgNamespace}}position").toString
    val pos = Position(posName) getOrElse Position.Unknown
    Part(Kanji.load(g), pos)
  }

}
