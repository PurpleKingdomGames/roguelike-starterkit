package roguelikestarterkit.ui.window

import indigo.shared.Outcome
import indigo.shared.events.GlobalEvent
import indigo.shared.scenegraph.Layer
import roguelikestarterkit.ComponentGroup
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.UiContext

trait WindowContent[A, ReferenceData]:

  /** Update this content's model.
    */
  def updateModel(
      context: UiContext[ReferenceData],
      model: A
  ): GlobalEvent => Outcome[A]

  /** Produce a renderable output for this content base on the model
    */
  def present(
      context: UiContext[ReferenceData],
      model: A
  ): Outcome[Layer]

  /** Called when the window's content area bounds changes, gives the model an opportunity to
    * respond to the new content area.
    */
  def cascade(model: A, newBounds: Bounds): A

object WindowContent:

  given [ReferenceData]: WindowContent[Unit, ReferenceData] with

    def updateModel(
        context: UiContext[ReferenceData],
        model: Unit
    ): GlobalEvent => Outcome[Unit] =
      _ => Outcome(model)

    def present(
        context: UiContext[ReferenceData],
        model: Unit
    ): Outcome[Layer] =
      Outcome(Layer.empty)

    def cascade(model: Unit, newBounds: Bounds): Unit =
      model

  given [ReferenceData]: WindowContent[ComponentGroup, ReferenceData] with

    def updateModel(
        context: UiContext[ReferenceData],
        model: ComponentGroup
    ): GlobalEvent => Outcome[ComponentGroup] =
      e => model.update(context)(e)

    def present(
        context: UiContext[ReferenceData],
        model: ComponentGroup
    ): Outcome[Layer] =
      model.present(context).map(_.toLayer)

    def cascade(model: ComponentGroup, newBounds: Bounds): ComponentGroup =
      model.cascade(newBounds)
