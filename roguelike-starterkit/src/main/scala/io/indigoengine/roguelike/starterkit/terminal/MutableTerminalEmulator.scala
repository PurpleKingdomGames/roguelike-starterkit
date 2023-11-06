package io.indigoengine.roguelike.starterkit.terminal

import indigo.*
import io.indigoengine.roguelike.starterkit.Tile

import scala.annotation.tailrec

import scalajs.js

final class MutableTerminalEmulator(
    val size: Size,
    tiles: js.Array[Tile],
    foreground: js.Array[RGBA],
    background: js.Array[RGBA]
):
  lazy val length: Int = size.width * size.height

  private def updateAt(
      index: Int,
      tile: Tile,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): Unit =
    tiles(index) = tile
    foreground(index) = foregroundColor
    background(index) = backgroundColor

  def put(
      coords: Point,
      tile: Tile,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): MutableTerminalEmulator =
    updateAt(
      MutableTerminalEmulator.pointToIndex(coords, size.width),
      tile,
      foregroundColor,
      backgroundColor
    )

    this

  def put(coords: Point, tile: Tile, foregroundColor: RGBA): MutableTerminalEmulator =
    put(coords, tile, foregroundColor, RGBA.Zero)

  def put(coords: Point, tile: Tile): MutableTerminalEmulator =
    put(coords, tile, RGBA.White, RGBA.Zero)

  def put(tiles: Batch[(Point, MapTile)]): MutableTerminalEmulator =
    tiles.foreach { t =>
      val idx = MutableTerminalEmulator.pointToIndex(t._1, size.width)
      val tt  = t._2

      updateAt(idx, tt.char, tt.foreground, tt.background)
    }

    this

  def put(tiles: Batch[(Point, MapTile)], offset: Point): MutableTerminalEmulator =
    tiles.foreach { t =>
      val idx = MutableTerminalEmulator.pointToIndex(t._1 + offset, size.width)
      val tt  = t._2

      updateAt(idx, tt.char, tt.foreground, tt.background)
    }

    this

  def put(tiles: (Point, MapTile)*): MutableTerminalEmulator =
    put(Batch.fromSeq(tiles))

  def put(coords: Point, mapTile: MapTile): MutableTerminalEmulator =
    put(coords, mapTile.char, mapTile.foreground, mapTile.background)

  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def fill(tile: Tile, foregroundColor: RGBA, backgroundColor: RGBA): MutableTerminalEmulator =
    val count = length
    var i     = 0

    while i < count do
      updateAt(i, tile, foregroundColor, backgroundColor)
      i += 1

    this

  def fill(mapTile: MapTile): MutableTerminalEmulator =
    fill(mapTile.char, mapTile.foreground, mapTile.background)

  def putLine(
      startCoords: Point,
      text: String,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): MutableTerminalEmulator =
    Batch.fromArray(text.toCharArray).zipWithIndex.foreach { case (c, i) =>
      val cc = Tile.charCodes.get(if c == '\\' then "\\" else c.toString)

      cc match
        case None =>
          ()

        case Some(char) =>
          startCoords + Point(i, 0) -> MapTile(Tile(char), foregroundColor, backgroundColor)
          updateAt(
            MutableTerminalEmulator.pointToIndex(startCoords + Point(i, 0), size.width),
            Tile(char),
            foregroundColor,
            backgroundColor
          )
    }

    this

  def putLines(
      startCoords: Point,
      textLines: Batch[String],
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): MutableTerminalEmulator =
    @tailrec
    def rec(remaining: List[String], yOffset: Int, term: MutableTerminalEmulator): MutableTerminalEmulator =
      remaining match
        case Nil =>
          term

        case x :: xs =>
          rec(
            xs,
            yOffset + 1,
            term.putLine(startCoords + Point(0, yOffset), x, foregroundColor, backgroundColor)
          )

    rec(textLines.toList, 0, this)

  def get(coords: Point): MapTile =
    val idx = MutableTerminalEmulator.pointToIndex(coords, size.width)
    val t   = tiles(idx)
    val f   = foreground(idx)
    val b   = background(idx)

    MapTile(t, f, b)

  def delete(coords: Point): MutableTerminalEmulator =
    put(coords, MutableTerminalEmulator.emptyTile)

  def clear: MutableTerminalEmulator =
    fill(MutableTerminalEmulator.emptyTile)

  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toTileBatch: Batch[MapTile] =
    val count = length
    var i     = 0
    val acc   = new js.Array[MapTile](count)

    while i < count do
      acc(i) = MapTile(tiles(i), foreground(i), background(i))
      i += 1

    Batch(acc)

  def draw(
      tileSheet: AssetName,
      charSize: Size,
      maxTileCount: Int
  ): TerminalEntity =
    TerminalEntity(tileSheet, size, charSize, toTileBatch, maxTileCount)

  private def toCloneTileData(
      position: Point,
      charCrops: Batch[(Int, Int, Int, Int)],
      data: Batch[(Point, MapTile)]
  ): Batch[CloneTileData] =
    data.map { case (pt, t) =>
      val crop = charCrops(t.char.toInt)
      CloneTileData(
        (pt.x * crop._3) + position.x,
        (pt.y * crop._4) + position.y,
        crop._1,
        crop._2,
        crop._3,
        crop._4
      )
    }

  def toCloneTiles(
      idPrefix: CloneId,
      position: Point,
      charCrops: Batch[(Int, Int, Int, Int)]
  )(makeBlank: (RGBA, RGBA) => Cloneable): TerminalClones =
    val makeId: (RGBA, RGBA) => CloneId = (fg, bg) =>
      CloneId(s"""${idPrefix.toString}_${fg.hashCode}_${bg.hashCode}""")

    val combinations: Batch[((CloneId, RGBA, RGBA), Batch[(Point, MapTile)])] =
      Batch.fromMap(
        toPositionedBatch
          .groupBy(p =>
            (makeId(p._2.foreground, p._2.background), p._2.foreground, p._2.background)
          )
      )

    val stuff =
      combinations.map { c =>
        (
          CloneBlank(c._1._1, makeBlank(c._1._2, c._1._3)),
          CloneTiles(c._1._1, toCloneTileData(position, charCrops, c._2))
        )
      }

    TerminalClones(stuff.map(_._1), stuff.map(_._2))

  def toBatch: Batch[MapTile] =
    toTileBatch

  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toPositionedBatch: Batch[(Point, MapTile)] =
    val count = length
    var i     = 0
    val acc   = new js.Array[(Point, MapTile)](count)

    while i < count do
      acc(i) = MutableTerminalEmulator.indexToPoint(i, size.width) -> MapTile(
        tiles(i),
        foreground(i),
        background(i)
      )
      i += 1

    Batch(acc)

  def |+|(otherConsole: MutableTerminalEmulator): MutableTerminalEmulator =
    combine(otherConsole)
  def combine(otherConsole: MutableTerminalEmulator): MutableTerminalEmulator =
    put(otherConsole.toPositionedBatch.filterNot(_._2 == MutableTerminalEmulator.emptyTile))

  def inset(otherConsole: MutableTerminalEmulator, offset: Point): MutableTerminalEmulator =
    put(otherConsole.toPositionedBatch.filterNot(_._2 == MutableTerminalEmulator.emptyTile), offset)

object MutableTerminalEmulator:

  private[terminal] lazy val emptyTile: MapTile = MapTile(Tile.NULL, RGBA.Zero, RGBA.Zero)

  inline def pointToIndex(point: Point, gridWidth: Int): Int =
    point.x + (point.y * gridWidth)

  inline def indexToPoint(index: Int, gridWidth: Int): Point =
    Point(
      x = index % gridWidth,
      y = index / gridWidth
    )

  def apply(size: Size): MutableTerminalEmulator =
    new MutableTerminalEmulator(
      size,
      new js.Array(size.width * size.height),
      new js.Array(size.width * size.height),
      new js.Array(size.width * size.height)
    ).fill(MutableTerminalEmulator.emptyTile)
