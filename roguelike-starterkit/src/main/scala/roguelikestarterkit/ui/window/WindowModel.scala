package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.CharSheet
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UiContext

final case class WindowModel[A](
    id: WindowId,
    charSheet: CharSheet,
    bounds: Bounds,
    title: Option[String],
    contentModel: A,
    updateContentModel: (UiContext, A) => GlobalEvent => Outcome[A],
    presentContentModel: (UiContext, A) => Outcome[SceneUpdateFragment],
    draggable: Boolean,
    resizable: Boolean,
    closeable: Boolean,
    hasFocus: Boolean,
    static: Boolean,
    minSize: Option[Dimensions],
    maxSize: Option[Dimensions],
    state: WindowState
):

  lazy val minAllowedSize: Dimensions =
    val m = if title.isEmpty then Dimensions(3) else Dimensions(3, 5)
    minSize.fold(m)(_.max(m))

  def withId(value: WindowId): WindowModel[A] =
    this.copy(id = value)

  def withBounds(value: Bounds): WindowModel[A] =
    this.copy(bounds = value)

  def withPosition(value: Coords): WindowModel[A] =
    withBounds(bounds.moveTo(value))
  def moveTo(position: Coords): WindowModel[A] =
    withPosition(position)
  def moveTo(x: Int, y: Int): WindowModel[A] =
    moveTo(Coords(x, y))
  def moveBy(amount: Coords): WindowModel[A] =
    withPosition(bounds.coords + amount)
  def moveBy(x: Int, y: Int): WindowModel[A] =
    moveBy(Coords(x, y))

  def withDimensions(value: Dimensions): WindowModel[A] =
    val d = value.max(minAllowedSize)
    withBounds(bounds.withDimensions(maxSize.fold(d)(_.min(d))))
  def resizeTo(size: Dimensions): WindowModel[A] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): WindowModel[A] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): WindowModel[A] =
    withDimensions(bounds.dimensions + amount)
  def resizeBy(x: Int, y: Int): WindowModel[A] =
    resizeBy(Dimensions(x, y))

  def withTitle(value: String): WindowModel[A] =
    this.copy(title = Option(value))

  def withModel(value: A): WindowModel[A] =
    this.copy(contentModel = value)

  def updateModel(
      f: (UiContext, A) => GlobalEvent => Outcome[A]
  ): WindowModel[A] =
    this.copy(updateContentModel = f)

  def present(
      f: (UiContext, A) => Outcome[SceneUpdateFragment]
  ): WindowModel[A] =
    this.copy(presentContentModel = f)

  def withDraggable(value: Boolean): WindowModel[A] =
    this.copy(draggable = value)
  def isDraggable: WindowModel[A] =
    withDraggable(true)
  def notDraggable: WindowModel[A] =
    withDraggable(false)

  def withResizable(value: Boolean): WindowModel[A] =
    this.copy(resizable = value)
  def isResizable: WindowModel[A] =
    withResizable(true)
  def notResizable: WindowModel[A] =
    withResizable(false)

  def withCloseable(value: Boolean): WindowModel[A] =
    this.copy(closeable = value)
  def isCloseable: WindowModel[A] =
    withCloseable(true)
  def notCloseable: WindowModel[A] =
    withCloseable(false)

  def withFocus(value: Boolean): WindowModel[A] =
    this.copy(hasFocus = value)
  def focus: WindowModel[A] =
    withFocus(true)
  def blur: WindowModel[A] =
    withFocus(false)

  def withStatic(value: Boolean): WindowModel[A] =
    this.copy(static = value)
  def isStatic: WindowModel[A] =
    withStatic(true)
  def notStatic: WindowModel[A] =
    withStatic(false)

  def withMinSize(min: Dimensions): WindowModel[A] =
    this.copy(minSize = Option(min))
  def withMinSize(width: Int, height: Int): WindowModel[A] =
    this.copy(minSize = Option(Dimensions(width, height)))
  def noMinSize: WindowModel[A] =
    this.copy(minSize = None)

  def withMaxSize(max: Dimensions): WindowModel[A] =
    this.copy(maxSize = Option(max))
  def withMaxSize(width: Int, height: Int): WindowModel[A] =
    this.copy(maxSize = Option(Dimensions(width, height)))
  def noMaxSize: WindowModel[A] =
    this.copy(maxSize = None)

  def withState(value: WindowState): WindowModel[A] =
    this.copy(state = value)
  def open: WindowModel[A] =
    withState(WindowState.Open)
  def close: WindowModel[A] =
    withState(WindowState.Closed)

  def isOpen: Boolean =
    state == WindowState.Open
  def isClosed: Boolean =
    state == WindowState.Closed

object WindowModel:

  def apply[A](
      id: WindowId,
      charSheet: CharSheet,
      content: A
  ): WindowModel[A] =
    WindowModel(
      id,
      charSheet,
      Bounds(Coords.zero, Dimensions.zero),
      None,
      contentModel = content,
      updateContentModel = (_, _) => _ => Outcome(content),
      presentContentModel = (_, _) => Outcome(SceneUpdateFragment.empty),
      false,
      false,
      false,
      false,
      false,
      None,
      None,
      WindowState.Closed
    )

  def apply[StartupData, CA](
      id: WindowId,
      charSheet: CharSheet
  ): WindowModel[Unit] =
    WindowModel(id, charSheet, ())
