package roguelikestarterkit.ui.components.list

import indigo.*
import roguelikestarterkit.ui.component.*
import roguelikestarterkit.ui.components.common.Anchor
import roguelikestarterkit.ui.components.common.ComponentEntry
import roguelikestarterkit.ui.components.common.ComponentId
import roguelikestarterkit.ui.components.common.ComponentLayout
import roguelikestarterkit.ui.components.common.ContainerLikeFunctions
import roguelikestarterkit.ui.components.common.Padding
import roguelikestarterkit.ui.datatypes.*
import ultraviolet.syntax.layout

import scala.annotation.tailrec

/** Describes a dynamic list of components, their realtive layout, and propagates update and
  * presention calls.
  */
final case class ComponentList[ReferenceData] private (
    content: ReferenceData => Batch[ComponentEntry[?, ReferenceData]],
    stateMap: Map[ComponentId, Any],
    layout: ComponentLayout,
    dimensions: Dimensions
):

  private def addSingle[A](entry: ReferenceData => (ComponentId, A))(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f =
      (r: ReferenceData) =>
        content(r) :+ {
          val (id, a) = entry(r)
          ComponentEntry(id, Coords.zero, a, c, Anchor.None)
        }

    this.copy(
      content = f
    )

  def addOne[A](entry: ReferenceData => (ComponentId, A))(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    addSingle(entry)

  def addOne[A](entry: (ComponentId, A))(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    addSingle(_ => entry)

  def add[A](entries: Batch[ReferenceData => (ComponentId, A)])(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    entries.foldLeft(this) { case (acc, next) => acc.addSingle(next) }

  def add[A](entries: (ReferenceData => (ComponentId, A))*)(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    Batch.fromSeq(entries).foldLeft(this) { case (acc, next) => acc.addSingle(next) }

  def add[A](entries: ReferenceData => Batch[(ComponentId, A)])(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    this.copy(
      content = (r: ReferenceData) =>
        content(r) ++ entries(r).map(v => ComponentEntry(v._1, Coords.zero, v._2, c, Anchor.None))
    )

  def withDimensions(value: Dimensions): ComponentList[ReferenceData] =
    this.copy(dimensions = value)

  def withLayout(value: ComponentLayout): ComponentList[ReferenceData] =
    this.copy(layout = value)

  def resizeTo(size: Dimensions): ComponentList[ReferenceData] =
    withDimensions(size)
  def resizeTo(x: Int, y: Int): ComponentList[ReferenceData] =
    resizeTo(Dimensions(x, y))
  def resizeBy(amount: Dimensions): ComponentList[ReferenceData] =
    withDimensions(dimensions + amount)
  def resizeBy(x: Int, y: Int): ComponentList[ReferenceData] =
    resizeBy(Dimensions(x, y))

object ComponentList:

  def apply[ReferenceData, A](
      dimensions: Dimensions
  )(contents: ReferenceData => Batch[(ComponentId, A)])(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f: ReferenceData => Batch[ComponentEntry[A, ReferenceData]] =
      r => contents(r).map(v => ComponentEntry(v._1, Coords.zero, v._2, c, Anchor.None))

    ComponentList(
      f,
      Map.empty,
      ComponentLayout.Vertical(Padding.zero),
      dimensions
    )

  def apply[ReferenceData, A](
      dimensions: Dimensions
  )(contents: (ComponentId, A)*)(using
      c: Component[A, ReferenceData]
  ): ComponentList[ReferenceData] =
    val f: ReferenceData => Batch[ComponentEntry[A, ReferenceData]] =
      _ => Batch.fromSeq(contents).map(v => ComponentEntry(v._1, Coords.zero, v._2, c, Anchor.None))

    ComponentList(
      f,
      Map.empty,
      ComponentLayout.Vertical(Padding.zero),
      dimensions
    )

  given [ReferenceData]: Component[ComponentList[ReferenceData], ReferenceData] with

    def bounds(
        context: ReferenceData,
        model: ComponentList[ReferenceData]
    ): Bounds =
      Bounds(model.dimensions)

    def updateModel(
        context: UIContext[ReferenceData],
        model: ComponentList[ReferenceData]
    ): GlobalEvent => Outcome[ComponentList[ReferenceData]] =
      case e =>
        // What we're doing here it updating the stateMap, not the content function.

        val nextStateMap =
          model
            .content(context.reference)
            .map { entry =>
              model.stateMap.get(entry.id) match
                case None =>
                  // No entry, so we make one based on the component's default state
                  entry.component.updateModel(context, entry.model)(e).map { newModel =>
                    entry.id -> newModel
                  }

                case Some(savedState) =>
                  // We have an entry, so we update it
                  entry.component.updateModel(context, savedState.asInstanceOf[entry.Out])(e).map {
                    newModel =>
                      entry.id -> newModel
                  }
            }
            .sequence
            .map(_.toMap)

        nextStateMap.map { newStateMap =>
          model.copy(stateMap = newStateMap)
        }

    def present(
        context: UIContext[ReferenceData],
        model: ComponentList[ReferenceData]
    ): Outcome[ComponentFragment] =
      // Pull the state out of the stateMap and present it
      val entries =
        model
          .content(context.reference)
          .map { entry =>
            model.stateMap.get(entry.id) match
              case None =>
                // No entry, so we use the default.
                entry

              case Some(savedState) =>
                // We have an entry, so overwrite the model with it.
                entry.copy(model = savedState.asInstanceOf[entry.Out])
          }

      ContainerLikeFunctions.present(
        context,
        contentReflow(context.reference, model.dimensions, model.layout, entries)
      )

    def refresh(
        reference: ReferenceData,
        model: ComponentList[ReferenceData],
        parentDimensions: Dimensions
    ): ComponentList[ReferenceData] =
      // Similar to updateModel, we're updating the stateMap, not the content function, but this time by invoking refresh

      val nextStateMap =
        model
          .content(reference)
          .map { entry =>
            model.stateMap.get(entry.id) match
              case None =>
                // No entry, so we make one based on the component's default state
                entry.id -> entry.component.refresh(reference, entry.model, parentDimensions)

              case Some(savedState) =>
                // We have an entry, so we update it
                entry.id -> entry.component.refresh(
                  reference,
                  savedState.asInstanceOf[entry.Out],
                  parentDimensions
                )
          }
          .toMap

      model.copy(stateMap = nextStateMap)

    private def contentReflow(
        reference: ReferenceData,
        dimensions: Dimensions,
        layout: ComponentLayout,
        entries: Batch[ComponentEntry[?, ReferenceData]]
    ): Batch[ComponentEntry[?, ReferenceData]] =
      val nextOffset =
        ContainerLikeFunctions
          .calculateNextOffset[ReferenceData](dimensions, layout)

      entries.foldLeft(Batch.empty[ComponentEntry[?, ReferenceData]]) { (acc, entry) =>
        val reflowed = entry.copy(
          offset = nextOffset(reference, acc)
        )

        acc :+ reflowed
      }
