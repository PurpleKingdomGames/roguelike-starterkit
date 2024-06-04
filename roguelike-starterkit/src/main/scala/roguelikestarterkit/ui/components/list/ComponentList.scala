package roguelikestarterkit.ui.components.list

import indigo.*
import roguelikestarterkit.Padding
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.components.group.ComponentLayout
import roguelikestarterkit.ui.datatypes.*

import scala.annotation.tailrec

/** Describes a dynamic list of 'stateless' components, their realtive layout, and propagates update
  * and presention calls.
  */
final case class ComponentList[ReferenceData] private (
    content: ReferenceData => Batch[ComponentListEntry[?, ReferenceData]],
    layout: ComponentLayout,
    bounds: Bounds
):

  private def addSingle[A](entry: ReferenceData => A)(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f =
      (r: ReferenceData) => content(r) :+ ComponentListEntry(Coords.zero, entry(r), c)

    this.copy(
      content = f
    )

  def addOne[A](entry: ReferenceData => A)(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    addSingle(entry)

  def addOne[A](entry: A)(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    addSingle(_ => entry)

  def add[A](entries: Batch[ReferenceData => A])(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    entries.foldLeft(this) { case (acc, next) => acc.addSingle(next) }

  def add[A](entries: (ReferenceData => A)*)(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    Batch.fromSeq(entries).foldLeft(this) { case (acc, next) => acc.addSingle(next) }

  def add[A](entries: ReferenceData => Batch[A])(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    this.copy(
      content = (r: ReferenceData) =>
        content(r) ++ entries(r).map(v => ComponentListEntry(Coords.zero, v, c))
    )

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
    val f: ReferenceData => Batch[ComponentListEntry[A, ReferenceData]] =
      r => contents(r).map(v => ComponentListEntry(Coords.zero, v, c))

    ComponentList(
      f,
      ComponentLayout.Vertical(Padding.zero),
      bounds
    )

  def apply[ReferenceData, A](
      bounds: Bounds
  )(contents: A*)(using
      c: StatelessComponent[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f: ReferenceData => Batch[ComponentListEntry[A, ReferenceData]] =
      _ => Batch.fromSeq(contents).map(v => ComponentListEntry(Coords.zero, v, c))

    ComponentList(
      f,
      ComponentLayout.Vertical(Padding.zero),
      bounds
    )

  given [ReferenceData]: StatelessComponent[ComponentList[ReferenceData], ReferenceData] with

    def bounds(
        context: ReferenceData,
        model: ComponentList[ReferenceData]
    ): Bounds =
      model.bounds

    def present(
        context: UiContext[ReferenceData],
        model: ComponentList[ReferenceData]
    ): Outcome[ComponentFragment] =
      contentReflow(context.reference, model)
        .map { c =>
          c.component.present(context.copy(bounds = context.bounds.moveBy(c.offset)), c.model)
        }
        .sequence
        .map(_.foldLeft(ComponentFragment.empty)(_ |+| _))

    private def contentReflow(
        reference: ReferenceData,
        model: ComponentList[ReferenceData]
    ): Batch[ComponentListEntry[?, ReferenceData]] =
      val nextOffset =
        ListFunctions.calculateNextOffset[ReferenceData](model.bounds, model.layout)

      model.content(reference).foldLeft(Batch.empty[ComponentListEntry[?, ReferenceData]]) {
        (acc, entry) =>
          val reflowed = entry.copy(
            offset = nextOffset(reference, acc)
          )

          acc :+ reflowed
      }
