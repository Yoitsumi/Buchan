package com.github.yoitsumi.buchan.model

import scala.collection.mutable

/**
 * Created by Kamil on 06.03.2016.
 */
sealed abstract class Position(val name: String) {
  override def toString = name

  Position.dict(name) = this

  lazy val ord = Position.values.indexOf(this)

}

object Position {

  case object Left extends Position("left")
  case object Right extends Position("right")
  case object Top extends Position("top")
  case object Bottom extends Position("bottom")
  case object Nyo extends Position("nyo")
  case object Tare extends Position("tare")
  case object Kamae extends Position("kamae")
  case object Kamae1 extends Position("kamae1")
  case object Kamae2 extends Position("kamae2")
  case object Unknown extends Position("unknown")

  private val dict = mutable.Map[String, Position]()

  val values = Seq(Left, Right, Top, Bottom, Nyo, Tare, Kamae, Kamae1, Kamae2, Unknown)

  def forName(n: String): Position = dict(n)

  def apply(name: String): Option[Position] = dict.get(name)

}
