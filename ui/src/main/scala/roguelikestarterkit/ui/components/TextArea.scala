package roguelikestarterkit.ui.components

import indigo.*
import indigo.syntax.*
import roguelikestarterkit.ui.component.Component
import roguelikestarterkit.ui.component.ComponentFragment
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UIContext

import scala.annotation.targetName

/** TextAreas are a simple `StatelessComponent` that render text.
  */
final case class TextArea[ReferenceData](
    text: ReferenceData => List[String],
    render: (Coords, List[String], Dimensions) => Outcome[ComponentFragment],
    calculateBounds: (ReferenceData, List[String]) => Bounds
):
  def withText(value: String): TextArea[ReferenceData] =
    this.copy(text = _ => value.split("\n").toList)
  def withText(f: ReferenceData => String): TextArea[ReferenceData] =
    this.copy(text = (r: ReferenceData) => f(r).split("\n").toList)

object TextArea:

  def apply[ReferenceData](text: String, calculateBounds: (ReferenceData, List[String]) => Bounds)(
      present: (Coords, List[String], Dimensions) => Outcome[ComponentFragment]
  ): TextArea[ReferenceData] =
    TextArea(
      (_: ReferenceData) => text.split("\n").toList,
      present,
      calculateBounds
    )

  @targetName("TextAreaRefToString")
  def apply[ReferenceData](
      text: ReferenceData => String,
      calculateBounds: (ReferenceData, List[String]) => Bounds
  )(
      present: (Coords, List[String], Dimensions) => Outcome[ComponentFragment]
  ): TextArea[ReferenceData] =
    TextArea(
      (r: ReferenceData) => text(r).split("\n").toList,
      present,
      calculateBounds
    )

  given [ReferenceData]: Component[TextArea[ReferenceData], ReferenceData] with
    def bounds(reference: ReferenceData, model: TextArea[ReferenceData]): Bounds =
      model.calculateBounds(reference, model.text(reference))

    def updateModel(
        context: UIContext[ReferenceData],
        model: TextArea[ReferenceData]
    ): GlobalEvent => Outcome[TextArea[ReferenceData]] =
      _ => Outcome(model)

    def present(
        context: UIContext[ReferenceData],
        model: TextArea[ReferenceData]
    ): Outcome[ComponentFragment] =
      model.render(
        context.bounds.coords,
        model.text(context.reference),
        bounds(context.reference, model).dimensions
      )

    def refresh(
        reference: ReferenceData,
        model: TextArea[ReferenceData],
        parentDimensions: Dimensions
    ): TextArea[ReferenceData] =
      model
