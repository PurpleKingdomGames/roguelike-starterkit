package roguelikestarterkit.ui.components.group

import indigo.*
import roguelikestarterkit.ui.component.Component
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.UiContext

/** `ComponentEntry`s record a components model, position, and relevant component typeclass instance
  * for use inside a `ComponentGroup`.
  */
final case class ComponentEntry[A, ReferenceData](
    offset: Coords,
    model: A,
    component: Component[A, ReferenceData]
):

  def cascade(
      context: UiContext[ReferenceData],
      parentBounds: Bounds
  ): ComponentEntry[A, ReferenceData] =
    this.copy(model = component.cascade(context, model, parentBounds))
