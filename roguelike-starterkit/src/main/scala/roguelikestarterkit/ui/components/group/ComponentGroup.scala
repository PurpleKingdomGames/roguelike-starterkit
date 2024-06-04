package roguelikestarterkit.ui.components.group

import indigo.*
import roguelikestarterkit.tiles.Tile.r
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.datatypes.*

import scala.annotation.tailrec

/** Describes a fixed arrangement of components, manages their layout, and propagates updates and
  * presention calls.
  */
final case class ComponentGroup[ReferenceData] private (
    boundsType: BoundsType,
    layout: ComponentLayout,
    components: Batch[ComponentGroupEntry[?, ReferenceData]],
    bounds: Bounds,
    contentBounds: Bounds,
    dirty: Boolean
):

  private def addSingle[A](entry: A)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    this.copy(
      components = components :+ ComponentGroupEntry(Coords.zero, entry, c),
      dirty = true
    )

  def add[A](entry: A)(using c: Component[A, ReferenceData]): ComponentGroup[ReferenceData] =
    addSingle(entry)

  def add[A](entries: Batch[A])(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    entries.foldLeft(this.copy(dirty = true)) { case (acc, next) => acc.addSingle(next) }
  def add[A](entries: A*)(using c: Component[A, ReferenceData]): ComponentGroup[ReferenceData] =
    add(Batch.fromSeq(entries))

  def withBounds(value: Bounds): ComponentGroup[ReferenceData] =
    this.copy(bounds = value, dirty = true)

  def withBoundsType(value: BoundsType): ComponentGroup[ReferenceData] =
    value match
      case BoundsType.Fixed(bounds) =>
        this.copy(boundsType = value, bounds = bounds, dirty = true)

      case _ =>
        this.copy(boundsType = value, dirty = true)

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
    this.copy(layout = value, dirty = true)

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

object ComponentGroup:

  def apply[ReferenceData](): ComponentGroup[ReferenceData] =
    ComponentGroup(
      BoundsType.default,
      ComponentLayout.Horizontal(Padding.zero, Overflow.Wrap),
      Batch.empty,
      Bounds.zero,
      Bounds.zero,
      dirty = true
    )

  def apply[ReferenceData](boundsType: BoundsType): ComponentGroup[ReferenceData] =
    ComponentGroup(
      boundsType,
      ComponentLayout.Horizontal(Padding.zero, Overflow.Wrap),
      Batch.empty,
      Bounds.zero,
      Bounds.zero,
      dirty = true
    )

  def apply[ReferenceData](bounds: Bounds): ComponentGroup[ReferenceData] =
    ComponentGroup(
      BoundsType.Fixed(bounds),
      ComponentLayout.Horizontal(Padding.zero, Overflow.Wrap),
      Batch.empty,
      bounds,
      Bounds.zero,
      dirty = true
    )

  given [ReferenceData]: Component[ComponentGroup[ReferenceData], ReferenceData] with

    def bounds(reference: ReferenceData, model: ComponentGroup[ReferenceData]): Bounds =
      model.bounds

    def updateModel(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
      case FrameTick =>
        updateComponents(context, model)(FrameTick).map { updated =>
          if model.dirty then
            val reflowed: ComponentGroup[ReferenceData] = reflow(context.reference, updated)
            val cascaded: ComponentGroup[ReferenceData] = cascade(reflowed, context.bounds)
            val contentBounds: Bounds =
              calculateContentBounds(context.reference, cascaded.components)

            cascaded.copy(
              dirty = false,
              contentBounds = contentBounds
            )
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
              components = updatedComponents
            )
          }

    private def calculateContentBounds[ReferenceData](
        reference: ReferenceData,
        components: Batch[ComponentGroupEntry[?, ReferenceData]]
    ): Bounds =
      components.foldLeft(Bounds.zero) { (acc, c) =>
        val bounds = c.component.bounds(reference, c.model).moveTo(c.offset)
        acc.expandToInclude(bounds)
      }

    def present(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): Outcome[ComponentFragment] =
      model.components
        .map { c =>
          c.component.present(context.copy(bounds = context.bounds.moveBy(c.offset)), c.model)
        }
        .sequence
        .map(_.foldLeft(ComponentFragment.empty)(_ |+| _))

    def reflow(
        reference: ReferenceData,
        model: ComponentGroup[ReferenceData]
    ): ComponentGroup[ReferenceData] =
      val reflowed: Batch[ComponentGroupEntry[?, ReferenceData]] = model.components.map { c =>
        c.copy(
          model = c.component.reflow(reference, c.model)
        )
      }

      val nextOffset =
        GroupFunctions.calculateNextOffset[ReferenceData](reference, model.bounds, model.layout)

      val newComponents = reflowed.foldLeft(Batch.empty[ComponentGroupEntry[?, ReferenceData]]) {
        (acc, entry) =>
          val reflowed = entry.copy(
            offset = nextOffset(acc),
            model = entry.component.reflow(reference, entry.model)
          )

          acc :+ reflowed
      }

      model.copy(
        components = newComponents
      )

    def cascade(
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
          components = model.components.map(_.cascade(newBounds)),
          dirty = true
        )
