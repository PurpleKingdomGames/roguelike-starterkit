package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.component.Component
import roguelikestarterkit.ui.components.common.Padding
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UIContext

/*

Missing stuff:

Components need a way to fill the available space.

Oh and we're still missing scrolling. Not totally sure where that goes yet. Clearly it's going to be important. I _think_ it's _probably_ and component group / list thing.

Might also need 'modal' windows that sit above everything.

Terminal components, supplying a string isn't always nice, would be good to allow a Batch[Tile] or something.

We need a standard window template. Title bar, close button, resize button.

One problem here is that if you want to use, say, a TextBox, then you need a BoundaryLocator instance. That comes from UIContext, but we can't have UIContext present as it makes the code untestable currently.

Package aliases.

A demo of a non-ASCII window.

 */

final case class Window[A, ReferenceData](
    id: WindowId,
    snapGrid: Size,
    bounds: Bounds,
    content: A,
    component: Component[A, ReferenceData],
    hasFocus: Boolean,
    minSize: Dimensions,
    maxSize: Option[Dimensions],
    state: WindowState,
    background: WindowContext => Outcome[Layer],
    mask: Padding
):

  def withId(value: WindowId): Window[A, ReferenceData] =
    this.copy(id = value)

  def withBounds(value: Bounds): Window[A, ReferenceData] =
    this.copy(bounds = value)

  def withPosition(value: Coords): Window[A, ReferenceData] =
    withBounds(bounds.moveTo(value))
  def moveTo(position: Coords): Window[A, ReferenceData] =
    withPosition(position)
  def moveTo(x: Int, y: Int): Window[A, ReferenceData] =
    moveTo(Coords(x, y))
  def moveBy(amount: Coords): Window[A, ReferenceData] =
    withPosition(bounds.coords + amount)
  def moveBy(x: Int, y: Int): Window[A, ReferenceData] =
    moveBy(Coords(x, y))

  def withDimensions(value: Dimensions): Window[A, ReferenceData] =
    val d = value.max(minSize)
    withBounds(bounds.withDimensions(maxSize.fold(d)(_.min(d))))
  def resizeTo(size: Dimensions): Window[A, ReferenceData] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): Window[A, ReferenceData] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): Window[A, ReferenceData] =
    withDimensions(bounds.dimensions + amount)
  def resizeBy(x: Int, y: Int): Window[A, ReferenceData] =
    resizeBy(Dimensions(x, y))

  def withModel(value: A): Window[A, ReferenceData] =
    this.copy(content = value)

  def withFocus(value: Boolean): Window[A, ReferenceData] =
    this.copy(hasFocus = value)
  def focus: Window[A, ReferenceData] =
    withFocus(true)
  def blur: Window[A, ReferenceData] =
    withFocus(false)

  def withMinSize(min: Dimensions): Window[A, ReferenceData] =
    this.copy(minSize = min)
  def withMinSize(width: Int, height: Int): Window[A, ReferenceData] =
    this.copy(minSize = Dimensions(width, height))

  def withMaxSize(max: Dimensions): Window[A, ReferenceData] =
    this.copy(maxSize = Option(max))
  def withMaxSize(width: Int, height: Int): Window[A, ReferenceData] =
    this.copy(maxSize = Option(Dimensions(width, height)))
  def noMaxSize: Window[A, ReferenceData] =
    this.copy(maxSize = None)

  def withState(value: WindowState): Window[A, ReferenceData] =
    this.copy(state = value)
  def open: Window[A, ReferenceData] =
    withState(WindowState.Open)
  def close: Window[A, ReferenceData] =
    withState(WindowState.Closed)

  def isOpen: Boolean =
    state == WindowState.Open
  def isClosed: Boolean =
    state == WindowState.Closed

  def withMaskPadding(value: Padding): Window[A, ReferenceData] =
    this.copy(mask = value)

  def refresh(reference: ReferenceData): Window[A, ReferenceData] =
    this.copy(content =
      component.refresh(
        reference,
        content,
        bounds.dimensions
      )
    )

  def withBackground(present: WindowContext => Outcome[Layer]): Window[A, ReferenceData] =
    this.copy(background = present)

object Window:

  def apply[A, ReferenceData](
      id: WindowId,
      snapGrid: Size,
      minSize: Dimensions,
      content: A
  )(using c: Component[A, ReferenceData]): Window[A, ReferenceData] =
    Window(
      id,
      snapGrid,
      Bounds(Coords.zero, Dimensions.zero),
      content,
      c,
      false,
      minSize,
      None,
      WindowState.Closed,
      _ => Outcome(Layer.empty),
      Padding.zero
    )

  def apply[A, ReferenceData](
      id: WindowId,
      snapGrid: Size,
      minSize: Dimensions,
      content: A
  )(
      background: WindowContext => Outcome[Layer]
  )(using c: Component[A, ReferenceData]): Window[A, ReferenceData] =
    Window(
      id,
      snapGrid,
      Bounds(Coords.zero, Dimensions.zero),
      content,
      c,
      false,
      minSize,
      None,
      WindowState.Closed,
      background,
      Padding.zero
    )

  def updateModel[A, ReferenceData](
      context: UIContext[ReferenceData],
      model: Window[A, ReferenceData]
  ): GlobalEvent => Outcome[Window[A, ReferenceData]] =
    case e =>
      model.component
        .updateModel(context.copy(bounds = model.bounds), model.content)(e)
        .map(model.withModel)

final case class WindowContext(
    bounds: Bounds,
    hasFocus: Boolean,
    mouseIsOver: Boolean,
    magnification: Int
)
object WindowContext:

  def from(model: Window[?, ?], viewModel: WindowViewModel[?]): WindowContext =
    WindowContext(
      model.bounds,
      model.hasFocus,
      viewModel.mouseIsOver,
      viewModel.magnification
    )
