package io.indigoengine.roguelike.starterkit.terminal

import indigo.*
import io.indigoengine.roguelike.starterkit.Tile

import scala.annotation.tailrec

import scalajs.js

/** `RogueTerminalEmulator` is like the `TerminalEmulator` but a little more daring and dangerous.
  * Represents an mutable, packed populated terminal. It is more performant, relative to
  * `TerminalEmulator`, but also requires more care since it's a mutable structure. There are no
  * empty spaces in this terminal, empty tiles are filled with the Tile.Null value and RGBA.Zero
  * colors.
  */
final class RogueTerminalEmulator(
    val size: Size,
    _tiles: js.Array[Tile],
    _foreground: js.Array[RGBA],
    _background: js.Array[RGBA]
) extends Terminal:
  lazy val length: Int                   = size.width * size.height
  lazy val tiles: Batch[Tile]            = Batch(_tiles.concat())
  lazy val foregroundColors: Batch[RGBA] = Batch(_foreground.concat())
  lazy val backgroundColors: Batch[RGBA] = Batch(_foreground.concat())

  override def clone(): RogueTerminalEmulator =
    new RogueTerminalEmulator(size, _tiles.concat(), _foreground.concat(), _background.concat())

  private def updateAt(
      index: Int,
      tile: Tile,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): Unit =
    _tiles(index) = tile
    _foreground(index) = foregroundColor
    _background(index) = backgroundColor

  def put(
      coords: Point,
      tile: Tile,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): RogueTerminalEmulator =
    updateAt(
      RogueTerminalEmulator.pointToIndex(coords, size.width),
      tile,
      foregroundColor,
      backgroundColor
    )

    this

  def put(coords: Point, tile: Tile, foregroundColor: RGBA): RogueTerminalEmulator =
    put(coords, tile, foregroundColor, RGBA.Zero)

  def put(coords: Point, tile: Tile): RogueTerminalEmulator =
    put(coords, tile, RGBA.White, RGBA.Zero)

  def put(tiles: Batch[(Point, MapTile)]): RogueTerminalEmulator =
    tiles.foreach { t =>
      val idx = RogueTerminalEmulator.pointToIndex(t._1, size.width)
      val tt  = t._2

      updateAt(idx, tt.char, tt.foreground, tt.background)
    }

    this

  def put(tiles: Batch[(Point, MapTile)], offset: Point): RogueTerminalEmulator =
    tiles.foreach { t =>
      val idx = RogueTerminalEmulator.pointToIndex(t._1 + offset, size.width)
      val tt  = t._2

      updateAt(idx, tt.char, tt.foreground, tt.background)
    }

    this

  def put(tiles: (Point, MapTile)*): RogueTerminalEmulator =
    put(Batch.fromSeq(tiles))

  def put(coords: Point, mapTile: MapTile): RogueTerminalEmulator =
    put(coords, mapTile.char, mapTile.foreground, mapTile.background)

  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def fill(tile: Tile, foregroundColor: RGBA, backgroundColor: RGBA): RogueTerminalEmulator =
    val count = length
    var i     = 0

    while i < count do
      updateAt(i, tile, foregroundColor, backgroundColor)
      i += 1

    this

  def fill(mapTile: MapTile): RogueTerminalEmulator =
    fill(mapTile.char, mapTile.foreground, mapTile.background)

  def putLine(
      startCoords: Point,
      text: String,
      foregroundColor: RGBA,
      backgroundColor: RGBA
  ): RogueTerminalEmulator =
    Batch.fromArray(text.toCharArray).zipWithIndex.foreach { case (c, i) =>
      val cc = Tile.charCodes.get(if c == '\\' then "\\" else c.toString)

      cc match
        case None =>
          ()

        case Some(char) =>
          startCoords + Point(i, 0) -> MapTile(Tile(char), foregroundColor, backgroundColor)
          updateAt(
            RogueTerminalEmulator.pointToIndex(startCoords + Point(i, 0), size.width),
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
  ): RogueTerminalEmulator =
    @tailrec
    def rec(
        remaining: List[String],
        yOffset: Int,
        term: RogueTerminalEmulator
    ): RogueTerminalEmulator =
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

  def get(coords: Point): Option[MapTile] =
    val idx = RogueTerminalEmulator.pointToIndex(coords, size.width)
    val t   = _tiles(idx)
    val f   = _foreground(idx)
    val b   = _background(idx)

    val mt = MapTile(t, f, b)
    if mt == Terminal.EmptyTile then None else Some(mt)

  def delete(coords: Point): RogueTerminalEmulator =
    put(coords, Terminal.EmptyTile)

  def clear: RogueTerminalEmulator =
    fill(Terminal.EmptyTile)

  def draw(
      tileSheet: AssetName,
      charSize: Size,
      maxTileCount: Int
  ): TerminalEntity =
    TerminalEntity(tileSheet, size, charSize, toTileBatch, maxTileCount)

  def draw(
      tileSheet: AssetName,
      charSize: Size,
      maxTileCount: Int,
      region: Rectangle
  ): TerminalEntity =
    TerminalEntity(tileSheet, size, charSize, toTileBatch(region), maxTileCount)

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

  private def toCloneTilesFromBatch(
      idPrefix: CloneId,
      position: Point,
      charCrops: Batch[(Int, Int, Int, Int)],
      positionedBatch: Batch[(Point, MapTile)],
      makeBlank: (RGBA, RGBA) => Cloneable
  ): TerminalClones =
    val makeId: (RGBA, RGBA) => CloneId = (fg, bg) =>
      CloneId(s"""${idPrefix.toString}_${fg.hashCode}_${bg.hashCode}""")

    val combinations: Batch[((CloneId, RGBA, RGBA), Batch[(Point, MapTile)])] =
      Batch.fromMap(
        positionedBatch
          .groupBy(p =>
            (makeId(p._2.foreground, p._2.background), p._2.foreground, p._2.background)
          )
      )

    val results =
      combinations.map { c =>
        (
          CloneBlank(c._1._1, makeBlank(c._1._2, c._1._3)),
          CloneTiles(c._1._1, toCloneTileData(position, charCrops, c._2))
        )
      }

    TerminalClones(results.map(_._1), results.map(_._2))

  /** Creates a `TerminalClones` instance of the given map. */
  def toCloneTiles(
      idPrefix: CloneId,
      position: Point,
      charCrops: Batch[(Int, Int, Int, Int)]
  )(makeBlank: (RGBA, RGBA) => Cloneable): TerminalClones =
    toCloneTilesFromBatch(idPrefix, position, charCrops, toPositionedBatch, makeBlank)

  /** Creates a `TerminalClones` instance of a defined region of the given map. */
  def toCloneTiles(
      idPrefix: CloneId,
      position: Point,
      charCrops: Batch[(Int, Int, Int, Int)],
      region: Rectangle
  )(makeBlank: (RGBA, RGBA) => Cloneable): TerminalClones =
    toCloneTilesFromBatch(idPrefix, position, charCrops, toPositionedBatch(region), makeBlank)

  /** Returns all MapTiles, guarantees order. */
  def toBatch: Batch[MapTile] =
    toTileBatch

  /** Returns all MapTiles in a given region, guarantees order. */
  def toBatch(region: Rectangle): Batch[MapTile] =
    toTileBatch(region)

  /** Returns all MapTiles, guarantees order. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toTileBatch: Batch[MapTile] =
    val count = length
    var i     = 0
    val acc   = new js.Array[MapTile](count)

    while i < count do
      acc(i) = MapTile(_tiles(i), _foreground(i), _background(i))
      i += 1

    Batch(acc)

  /** Returns all MapTiles in a given region, guarantees order. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toTileBatch(region: Rectangle): Batch[MapTile] =
    val count = length
    var i     = 0
    var j     = 0
    val acc   = new js.Array[MapTile](region.size.width * region.size.height)

    while i < count do
      if region.contains(RogueTerminalEmulator.indexToPoint(i, size.width)) then
        acc(j) = MapTile(_tiles(i), _foreground(i), _background(i))
        j += 1
      i += 1

    Batch(acc)

  /** Returns all MapTiles with their grid positions, guarantees order.
    */
  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toPositionedBatch: Batch[(Point, MapTile)] =
    val count = length
    var i     = 0
    val acc   = new js.Array[(Point, MapTile)](count)

    while i < count do
      acc(i) = RogueTerminalEmulator.indexToPoint(i, size.width) -> MapTile(
        _tiles(i),
        _foreground(i),
        _background(i)
      )
      i += 1

    Batch(acc)

  /** Returns all MapTiles with their grid positions in a given region, guarantees order.
    */
  @SuppressWarnings(Array("scalafix:DisableSyntax.while", "scalafix:DisableSyntax.var"))
  def toPositionedBatch(region: Rectangle): Batch[(Point, MapTile)] =
    val count = length
    var i     = 0
    var j     = 0
    val acc   = new js.Array[(Point, MapTile)](region.size.width * region.size.height)

    while i < count do
      val pt = RogueTerminalEmulator.indexToPoint(i, size.width)
      if region.contains(pt) then
        acc(j) = pt -> MapTile(
          _tiles(i),
          _foreground(i),
          _background(i)
        )
        j += 1
      i += 1

    Batch(acc)

  def |+|(otherConsole: Terminal): RogueTerminalEmulator =
    combine(otherConsole)
  def combine(otherConsole: Terminal): RogueTerminalEmulator =
    put(otherConsole.toPositionedBatch.filterNot(_._2 == Terminal.EmptyTile))

  def inset(otherConsole: Terminal, offset: Point): RogueTerminalEmulator =
    put(otherConsole.toPositionedBatch.filterNot(_._2 == Terminal.EmptyTile), offset)

  def toTerminalEmulator: TerminalEmulator =
    TerminalEmulator(size).inset(this, Point.zero)

object RogueTerminalEmulator:

  inline def pointToIndex(point: Point, gridWidth: Int): Int =
    point.x + (point.y * gridWidth)

  inline def indexToPoint(index: Int, gridWidth: Int): Point =
    Point(
      x = index % gridWidth,
      y = index / gridWidth
    )

  def apply(size: Size): RogueTerminalEmulator =
    new RogueTerminalEmulator(
      size,
      new js.Array(size.width * size.height),
      new js.Array(size.width * size.height),
      new js.Array(size.width * size.height)
    ).fill(Terminal.EmptyTile)
