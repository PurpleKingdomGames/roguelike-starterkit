package roguelikestarterkit.ui.components.group

import indigo.*
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.datatypes.*

import scala.annotation.tailrec

/** Describes a dynamic list of 'stateless' components, their realtive layout, and propagates update
  * and presention calls.
  */
final case class ComponentList[ReferenceData] private (
    content: ReferenceData => Batch[ComponentEntry[?, ReferenceData]],
    layout: ComponentLayout,
    components: Batch[ComponentEntry[?, ReferenceData]],
    bounds: Bounds
):

  // This is private and is only used for testing
  private[group] def withComponents(
      components: Batch[ComponentEntry[?, ReferenceData]]
  ): ComponentList[ReferenceData] =
    this.copy(components = components)

  def withBounds(value: Bounds): ComponentList[ReferenceData] =
    this.copy(bounds = value)

  def withLayout(value: ComponentLayout): ComponentList[ReferenceData] =
    this.copy(layout = value)

  def withPosition(value: Coords): ComponentList[ReferenceData] =
    withBounds(bounds.withPosition(value))
  def moveTo(position: Coords): ComponentList[ReferenceData] =
    withPosition(position)
  def moveTo(x: Int, y: Int): ComponentList[ReferenceData] =
    moveTo(Coords(x, y))
  def moveBy(amount: Coords): ComponentList[ReferenceData] =
    withPosition(bounds.coords + amount)
  def moveBy(x: Int, y: Int): ComponentList[ReferenceData] =
    moveBy(Coords(x, y))

  def withDimensions(value: Dimensions): ComponentList[ReferenceData] =
    withBounds(bounds.withDimensions(value))
  def resizeTo(size: Dimensions): ComponentList[ReferenceData] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): ComponentList[ReferenceData] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): ComponentList[ReferenceData] =
    withDimensions(bounds.dimensions + amount)
  def resizeBy(x: Int, y: Int): ComponentList[ReferenceData] =
    resizeBy(Dimensions(x, y))

object ComponentList:

  def apply[ReferenceData, A](
      bounds: Bounds
  )(contents: ReferenceData => Batch[A])(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f: ReferenceData => Batch[ComponentEntry[A, ReferenceData]] =
      r => contents(r).map(v => ComponentEntry(Coords.zero, v, c))

    ComponentList(
      f,
      ComponentLayout.None,
      Batch.empty,
      bounds
    )

  given [ReferenceData]: Component[ComponentList[ReferenceData], ReferenceData] with

    def bounds(model: ComponentList[ReferenceData]): Bounds =
      model.bounds

    def updateModel(
        context: UiContext[ReferenceData],
        model: ComponentList[ReferenceData]
    ): GlobalEvent => Outcome[ComponentList[ReferenceData]] =
      case e => Outcome(model)

    def present(
        context: UiContext[ReferenceData],
        model: ComponentList[ReferenceData]
    ): Outcome[ComponentFragment] =
      GroupFunctions.present(
        context,
        reflow(model.withComponents(model.content(context.reference))).components
      )

    def reflow(model: ComponentList[ReferenceData]): ComponentList[ReferenceData] =
      val nextOffset =
        GroupFunctions.calculateNextOffset[ReferenceData](model.bounds, model.layout)

      val newComponents =
        model.components.foldLeft(Batch.empty[ComponentEntry[?, ReferenceData]]) { (acc, entry) =>
          val reflowed = entry.copy(
            offset = nextOffset(acc)
          )

          acc :+ reflowed
        }

      model.copy(
        components = newComponents
      )

    def cascade(
        model: ComponentList[ReferenceData],
        parentBounds: Bounds
    ): ComponentList[ReferenceData] =
      model
