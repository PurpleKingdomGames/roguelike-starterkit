package roguelikestarterkit.ui.components.group

import indigo.*
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.datatypes.*

import scala.annotation.tailrec

/** Describes a fixed arrangement of components, manages their layout, and propagates updates and
  * presention calls.
  */
final case class ComponentGroup[ReferenceData] private (
    boundsType: BoundsType,
    layout: ComponentLayout,
    components: Batch[ComponentEntry[?, ReferenceData]],
    bounds: Bounds,
    referenceBounds: Batch[Bounds]
):

  lazy val contentBounds: Bounds =
    val allBounds: Batch[Rectangle] = referenceBounds.map(_.unsafeToRectangle)

    if allBounds.isEmpty then Bounds.zero
    else
      val h = allBounds.head
      val t = allBounds.tail

      Bounds(t.foldLeft(h) { case (acc, r) => acc.expandToInclude(r) })

  private def addSingle[A](entry: A)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    this.copy(
      components = components :+ ComponentEntry(Coords.zero, entry, c),
      referenceBounds = referenceBounds :+ Bounds.zero
    )

  def add[A](entry: A)(using c: Component[A, ReferenceData]): ComponentGroup[ReferenceData] =
    addSingle(entry)

  def add[A](entries: Batch[A])(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    entries.foldLeft(this) { case (acc, next) => acc.addSingle(next) }
  def add[A](entries: A*)(using c: Component[A, ReferenceData]): ComponentGroup[ReferenceData] =
    add(Batch.fromSeq(entries))

  def withBounds(value: Bounds): ComponentGroup[ReferenceData] =
    this.copy(bounds = value)

  def withBoundsType(value: BoundsType): ComponentGroup[ReferenceData] =
    value match
      case BoundsType.Fixed(bounds) =>
        this.copy(boundsType = value, bounds = bounds)

      case _ =>
        this.copy(boundsType = value)

  def defaultBounds: ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.default)
  def fixedBounds(value: Bounds): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.Fixed(value))
  def inheritBounds: ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.Inherit)
  def relative(x: Double, y: Double, width: Double, height: Double): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.Relative(x, y, width, height))
  def relativePosition(x: Double, y: Double): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.RelativePosition(x, y))
  def relativeSize(width: Double, height: Double): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.RelativeSize(width, height))
  def offset(amountPosition: Coords, amountSize: Dimensions): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.Offset(amountPosition, amountSize))
  def offset(x: Int, y: Int, width: Int, height: Int): ComponentGroup[ReferenceData] =
    offset(Coords(x, y), Dimensions(width, height))
  def offsetPosition(amount: Coords): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.OffsetPosition(amount))
  def offsetPosition(x: Int, y: Int): ComponentGroup[ReferenceData] =
    offsetPosition(Coords(x, y))
  def offsetSize(amount: Dimensions): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.OffsetSize(amount))
  def offsetSize(width: Int, height: Int): ComponentGroup[ReferenceData] =
    offsetSize(Dimensions(width, height))
  def dynamicBounds(width: FitMode, height: FitMode): ComponentGroup[ReferenceData] =
    withBoundsType(BoundsType.Dynamic(width, height))

  def withLayout(value: ComponentLayout): ComponentGroup[ReferenceData] =
    this.copy(layout = value)

  def withPosition(value: Coords): ComponentGroup[ReferenceData] =
    withBounds(bounds.withPosition(value))
  def moveTo(position: Coords): ComponentGroup[ReferenceData] =
    withPosition(position)
  def moveTo(x: Int, y: Int): ComponentGroup[ReferenceData] =
    moveTo(Coords(x, y))
  def moveBy(amount: Coords): ComponentGroup[ReferenceData] =
    withPosition(bounds.coords + amount)
  def moveBy(x: Int, y: Int): ComponentGroup[ReferenceData] =
    moveBy(Coords(x, y))

  def withDimensions(value: Dimensions): ComponentGroup[ReferenceData] =
    withBounds(bounds.withDimensions(value))
  def resizeTo(size: Dimensions): ComponentGroup[ReferenceData] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): ComponentGroup[ReferenceData] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): ComponentGroup[ReferenceData] =
    withDimensions(bounds.dimensions + amount)
  def resizeBy(x: Int, y: Int): ComponentGroup[ReferenceData] =
    resizeBy(Dimensions(x, y))

  // Delegates, for convenience.

  def update[StartupData, ContextData](
      context: UiContext[ReferenceData]
  ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]].updateModel(context, this)

  def present[StartupData, ContextData](
      context: UiContext[ReferenceData]
  ): Outcome[ComponentFragment] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]].present(context, this)

  def reflow(
      context: UiContext[ReferenceData]
  ): ComponentGroup[ReferenceData] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]].reflow(context, this)

  def cascade(
      context: UiContext[ReferenceData],
      parentBounds: Bounds
  ): ComponentGroup[ReferenceData] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]]
      .cascade(context, this, parentBounds)

object ComponentGroup:

  def apply[ReferenceData](): ComponentGroup[ReferenceData] =
    ComponentGroup(
      BoundsType.default,
      ComponentLayout.None,
      Batch.empty,
      Bounds.zero,
      Batch.empty
    )

  def apply[ReferenceData](boundsType: BoundsType): ComponentGroup[ReferenceData] =
    ComponentGroup(
      boundsType,
      ComponentLayout.None,
      Batch.empty,
      Bounds.zero,
      Batch.empty
    )

  def apply[ReferenceData](bounds: Bounds): ComponentGroup[ReferenceData] =
    ComponentGroup(
      BoundsType.Fixed(bounds),
      ComponentLayout.None,
      Batch.empty,
      bounds,
      Batch.empty
    )

  given [ReferenceData]: Component[ComponentGroup[ReferenceData], ReferenceData] with

    def bounds(context: UiContext[ReferenceData], model: ComponentGroup[ReferenceData]): Bounds =
      model.bounds

    def updateModel(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
      case FrameTick =>
        updateComponents(context, model)(FrameTick).map { updated =>
          if updated.referenceBounds != model.referenceBounds then updated.reflow(context)
          else updated
        }

      case e =>
        updateComponents(context, model)(e)

    private def updateComponents[StartupData, ContextData](
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
      e =>
        model.components
          .map { c =>
            c.component
              .updateModel(context.copy(bounds = context.bounds.moveBy(c.offset)), c.model)(e)
              .map { updated =>
                c.copy(model = updated)
              }
          }
          .sequence
          .map { updatedComponents =>
            model.copy(
              components = updatedComponents,
              referenceBounds = updatedComponents.map { c =>
                c.component.bounds(context, c.model).moveTo(c.offset)
              }
            )
          }

    def present(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): Outcome[ComponentFragment] =
      GroupFunctions.present(context, model.components)

    def reflow(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): ComponentGroup[ReferenceData] =
      val reflowed: Batch[ComponentEntry[?, ReferenceData]] = model.components.map { c =>
        c.copy(
          model = c.component.reflow(context, c.model)
        )
      }

      val nextOffset =
        GroupFunctions.calculateNextOffset[ReferenceData](context, model.bounds, model.layout)

      val newComponents = reflowed.foldLeft(Batch.empty[ComponentEntry[?, ReferenceData]]) {
        (acc, entry) =>
          val reflowed = entry.copy(
            offset = nextOffset(acc),
            model = entry.component.reflow(context, entry.model)
          )

          acc :+ reflowed
      }

      val newReferenceBounds =
        newComponents.map(c => c.component.bounds(context, c.model).moveTo(c.offset))

      model.copy(
        components = newComponents,
        referenceBounds = newReferenceBounds
      )

    def cascade(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData],
        parentBounds: Bounds
    ): ComponentGroup[ReferenceData] =
      val newBounds: Bounds =
        GroupFunctions.calculateCascadeBounds(
          model.bounds,
          model.contentBounds,
          parentBounds,
          model.boundsType
        )

      model
        .copy(
          bounds = newBounds,
          components = model.components.map(_.cascade(context, newBounds))
        )
