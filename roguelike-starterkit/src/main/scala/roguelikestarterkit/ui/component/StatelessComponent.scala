package roguelikestarterkit.ui.component

import indigo.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.UiContext

/** A typeclass that confirms that some type `A` can be used as a `Component` provides the necessary
  * operations for that type to act as a component.
  */
trait StatelessComponent[A, ReferenceData] extends Component[A, ReferenceData]:

  /** The position and size of the component
    */
  def bounds(context: UiContext[ReferenceData], model: A): Bounds

  /** Handle an event that has been dispatched to this component.
    */
  def handleEvent(
      context: UiContext[ReferenceData],
      model: A
  ): GlobalEvent => Batch[GlobalEvent]

  /** Produce a renderable output for this component, based on the component's model.
    */
  def present(
      context: UiContext[ReferenceData],
      model: A
  ): Outcome[ComponentFragment]

  def updateModel(context: UiContext[ReferenceData], model: A): GlobalEvent => Outcome[A] =
    case e => Outcome(model).addGlobalEvents(handleEvent(context, model)(e))

  def reflow(context: UiContext[ReferenceData], model: A): A                        = model
  def cascade(context: UiContext[ReferenceData], model: A, parentBounds: Bounds): A = model
