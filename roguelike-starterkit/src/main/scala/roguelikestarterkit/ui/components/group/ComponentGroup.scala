package roguelikestarterkit.ui.components.group

import indigo.*
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.datatypes.*

import scala.annotation.tailrec

/** Encapsulates a collection of components and describes and manages their layout, as well as
  * propagating update and presention calls.
  */
final case class ComponentGroup[ReferenceData] private (
    // Config
    initalReferenceData: ReferenceData,
    content: ReferenceData => Batch[ComponentEntry[?, ReferenceData]],
    boundsType: BoundsType,
    layout: ComponentLayout,
    // View model data
    contentHash: Int,
    computedComponents: Batch[ComponentEntry[?, ReferenceData]],
    computedBounds: Bounds,
    referenceBounds: Batch[Bounds]
):

  lazy val contentBounds: Bounds =
    val allBounds: Batch[Rectangle] = referenceBounds.map(_.unsafeToRectangle)

    if allBounds.isEmpty then Bounds.zero
    else
      val h = allBounds.head
      val t = allBounds.tail

      Bounds(t.foldLeft(h) { case (acc, r) => acc.expandToInclude(r) })

  private def addSingle[A](entry: ReferenceData => A)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    val init = ComponentEntry(
      ComponentGroup.calculateNextOffset(computedBounds, layout)(computedComponents),
      entry(initalReferenceData),
      c
    )

    this.copy(
      content = (r: ReferenceData) =>
        content(r) :+ ComponentEntry(
          ComponentGroup.calculateNextOffset(computedBounds, layout)(computedComponents),
          entry(r),
          c
        ),
      computedComponents = computedComponents :+ init,
      referenceBounds = referenceBounds :+ Bounds.zero
    )

  def addDynamic[A](entry: ReferenceData => A)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    addSingle(entry).reflow.cascade(computedBounds)
  def add[A](entry: A)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    addDynamic((_: ReferenceData) => entry)

  def addDynamic[A](entries: Batch[ReferenceData => A])(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    entries
      .foldLeft(this) { case (acc, next) => acc.addSingle(next) }
      .reflow
      .cascade(computedBounds)
  def add[A](entries: Batch[A])(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    addDynamic(entries.map(e => (_: ReferenceData) => e))

  def addDynamic[A](entries: (ReferenceData => A)*)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    addDynamic(Batch.fromSeq(entries))
  def add[A](entries: A*)(using
      c: Component[A, ReferenceData]
  ): ComponentGroup[ReferenceData] =
    addDynamic(Batch.fromSeq(entries).map(e => (_: ReferenceData) => e))

  def withBounds(value: Bounds): ComponentGroup[ReferenceData] =
    this.copy(computedBounds = value).reflow

  def withBoundsType(value: BoundsType): ComponentGroup[ReferenceData] =
    this.copy(boundsType = value).reflow

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
    this.copy(layout = value).reflow

  def withPosition(value: Coords): ComponentGroup[ReferenceData] =
    withBounds(computedBounds.withPosition(value))
  def moveTo(position: Coords): ComponentGroup[ReferenceData] =
    withPosition(position)
  def moveTo(x: Int, y: Int): ComponentGroup[ReferenceData] =
    moveTo(Coords(x, y))
  def moveBy(amount: Coords): ComponentGroup[ReferenceData] =
    withPosition(computedBounds.coords + amount)
  def moveBy(x: Int, y: Int): ComponentGroup[ReferenceData] =
    moveBy(Coords(x, y))

  def withDimensions(value: Dimensions): ComponentGroup[ReferenceData] =
    withBounds(computedBounds.withDimensions(value))
  def resizeTo(size: Dimensions): ComponentGroup[ReferenceData] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): ComponentGroup[ReferenceData] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): ComponentGroup[ReferenceData] =
    withDimensions(computedBounds.dimensions + amount)
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

  def reflow: ComponentGroup[ReferenceData] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]].reflow(this)

  def cascade(parentBounds: Bounds): ComponentGroup[ReferenceData] =
    summon[Component[ComponentGroup[ReferenceData], ReferenceData]].cascade(this, parentBounds)

object ComponentGroup:

  def apply(): ComponentGroup[Unit] =
    ComponentGroup(
      (),
      _ => Batch.empty,
      BoundsType.default,
      ComponentLayout.None,
      -1,
      Batch.empty,
      Bounds.zero,
      Batch.empty
    )

  def apply[ReferenceData](initialReferenceData: ReferenceData): ComponentGroup[ReferenceData] =
    ComponentGroup(
      initialReferenceData,
      _ => Batch.empty,
      BoundsType.default,
      ComponentLayout.None,
      -1,
      Batch.empty,
      Bounds.zero,
      Batch.empty
    )

  def apply[ReferenceData](
      bounds: Bounds
  ): ComponentGroup[Unit] =
    ComponentGroup(
      (),
      _ => Batch.empty,
      BoundsType.Fixed(bounds),
      ComponentLayout.None,
      -1,
      Batch.empty,
      bounds,
      Batch.empty
    )

  def apply[ReferenceData](
      initialReferenceData: ReferenceData,
      bounds: Bounds
  ): ComponentGroup[ReferenceData] =
    ComponentGroup(
      initialReferenceData,
      _ => Batch.empty,
      BoundsType.Fixed(bounds),
      ComponentLayout.None,
      -1,
      Batch.empty,
      bounds,
      Batch.empty
    )

  extension [A <: Any](batch: Batch[A])
    @SuppressWarnings(Array("scalafix:DisableSyntax.null", "scalafix:DisableSyntax.var"))
    def customHashCode: Int =
      val prime  = 31
      var result = 1
      for (element <- batch)
        result = prime * result + (if (element == null) 0 else element.hashCode)
      result

  given [ReferenceData]: Component[ComponentGroup[ReferenceData], ReferenceData] with

    def bounds(model: ComponentGroup[ReferenceData]): Bounds =
      model.computedBounds

    def updateModel(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
      case FrameTick =>
        updateComponents(context, model)(FrameTick).map { updated =>
          if updated.referenceBounds != model.referenceBounds then updated.reflow
          else updated
        }

      case e =>
        updateComponents(context, model)(e)

    private def updateComponents[StartupData, ContextData](
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): GlobalEvent => Outcome[ComponentGroup[ReferenceData]] =
      e =>
        val entries = model.content(context.reference)

        val modelWithComponents =
          if entries.customHashCode == model.contentHash then model
          else
            model
              .copy(computedComponents = entries, contentHash = entries.customHashCode)
              .reflow
              .cascade(model.computedBounds)

        modelWithComponents.computedComponents
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
              computedComponents = updatedComponents,
              referenceBounds = updatedComponents.map { c =>
                c.component.bounds(c.model)
              }
            )
          }

    def present(
        context: UiContext[ReferenceData],
        model: ComponentGroup[ReferenceData]
    ): Outcome[ComponentFragment] =
      model.computedComponents
        .map { c =>
          c.component.present(context.copy(bounds = context.bounds.moveBy(c.offset)), c.model)
        }
        .sequence
        .map(_.foldLeft(ComponentFragment.empty)(_ |+| _))

    def reflow(model: ComponentGroup[ReferenceData]): ComponentGroup[ReferenceData] =
      val reflowed: Batch[ComponentEntry[?, ReferenceData]] = model.computedComponents.map { c =>
        c.copy(
          model = c.component.reflow(c.model)
        )
      }

      val nextOffset = calculateNextOffset[ReferenceData](model.computedBounds, model.layout)

      val newComponents = reflowed.foldLeft(Batch.empty[ComponentEntry[?, ReferenceData]]) {
        (acc, entry) =>
          val reflowed = entry.copy(
            offset = nextOffset(acc),
            model = entry.component.reflow(entry.model)
          )

          acc :+ reflowed
      }

      val newReferenceBounds =
        newComponents.map(c => c.component.bounds(c.model).withPosition(c.offset))

      model.copy(
        computedComponents = newComponents,
        referenceBounds = newReferenceBounds
      )

    def cascade(
        model: ComponentGroup[ReferenceData],
        parentBounds: Bounds
    ): ComponentGroup[ReferenceData] =
      val newBounds: Bounds =
        model.boundsType match
          case BoundsType.Fixed(b) =>
            b

          case BoundsType.Inherit =>
            parentBounds

          case BoundsType.Relative(x, y, width, height) =>
            Bounds(
              (parentBounds.width.toDouble * x).toInt,
              (parentBounds.height.toDouble * y).toInt,
              (parentBounds.width.toDouble * width).toInt,
              (parentBounds.height.toDouble * height).toInt
            )

          case BoundsType.RelativePosition(x, y) =>
            model.computedBounds.withPosition(
              (parentBounds.width.toDouble * x).toInt,
              (parentBounds.height.toDouble * y).toInt
            )

          case BoundsType.RelativeSize(width, height) =>
            model.computedBounds.withDimensions(
              (parentBounds.width.toDouble * width).toInt,
              (parentBounds.height.toDouble * height).toInt
            )

          case BoundsType.Offset(amountPosition, amountSize) =>
            Bounds(parentBounds.coords + amountPosition, parentBounds.dimensions + amountSize)

          case BoundsType.OffsetPosition(amount) =>
            model.computedBounds.withPosition(parentBounds.coords + amount)

          case BoundsType.OffsetSize(amount) =>
            model.computedBounds.withDimensions(parentBounds.dimensions + amount)

          case BoundsType.Dynamic(FitMode.Available, FitMode.Available) =>
            parentBounds

          case BoundsType.Dynamic(FitMode.Available, FitMode.Content) =>
            parentBounds.withDimensions(
              parentBounds.dimensions.width,
              model.contentBounds.dimensions.height
            )

          case BoundsType.Dynamic(FitMode.Available, FitMode.Fixed(units)) =>
            parentBounds.withDimensions(parentBounds.dimensions.width, units)

          case BoundsType.Dynamic(FitMode.Content, FitMode.Available) =>
            parentBounds.withDimensions(
              model.contentBounds.dimensions.height,
              parentBounds.dimensions.width
            )

          case BoundsType.Dynamic(FitMode.Content, FitMode.Content) =>
            model.contentBounds

          case BoundsType.Dynamic(FitMode.Content, FitMode.Fixed(units)) =>
            model.contentBounds.withDimensions(model.contentBounds.dimensions.width, units)

          case BoundsType.Dynamic(FitMode.Fixed(units), FitMode.Available) =>
            parentBounds.withDimensions(units, parentBounds.dimensions.height)

          case BoundsType.Dynamic(FitMode.Fixed(units), FitMode.Content) =>
            model.contentBounds.withDimensions(units, model.contentBounds.dimensions.height)

          case BoundsType.Dynamic(FitMode.Fixed(unitsW), FitMode.Fixed(unitsH)) =>
            model.computedBounds.withDimensions(unitsW, unitsH)

      model
        .copy(
          computedBounds = newBounds,
          computedComponents = model.computedComponents.map(_.cascade(newBounds))
        )

  extension (b: Bounds)
    private def withPadding(p: Padding): Bounds =
      b.moveBy(p.left, p.top).resize(b.width + p.right, b.height + p.bottom)

  private def calculateNextOffset[ReferenceData](bounds: Bounds, layout: ComponentLayout)(
      components: Batch[ComponentEntry[?, ReferenceData]]
  ): Coords =
    layout match
      case ComponentLayout.None =>
        Coords.zero

      case ComponentLayout.Horizontal(padding, Overflow.Hidden) =>
        components
          .takeRight(1)
          .headOption
          .map(c => c.offset + Coords(c.component.bounds(c.model).withPadding(padding).right, 0))
          .getOrElse(Coords(padding.left, padding.top))

      case ComponentLayout.Horizontal(padding, Overflow.Wrap) =>
        val maxY = components
          .map(c => c.offset.y + c.component.bounds(c.model).withPadding(padding).height)
          .sortWith(_ > _)
          .headOption
          .getOrElse(0)

        components
          .takeRight(1)
          .headOption
          .map { c =>
            val padded      = c.component.bounds(c.model).withPadding(padding)
            val maybeOffset = c.offset + Coords(padded.right, 0)

            if padded.moveBy(maybeOffset).right < bounds.width then maybeOffset
            else Coords(padding.left, maxY)
          }
          .getOrElse(Coords(padding.left, padding.top))

      case ComponentLayout.Vertical(padding) =>
        components
          .takeRight(1)
          .headOption
          .map(c => c.offset + Coords(0, c.component.bounds(c.model).withPadding(padding).bottom))
          .getOrElse(Coords(padding.left, padding.top))
