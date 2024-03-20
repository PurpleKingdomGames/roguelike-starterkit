package roguelikestarterkit.ui.component

import indigo.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.UiContext

trait Component[A]:

  def bounds(model: A): Bounds

  def updateModel(
      context: UiContext,
      model: A
  ): GlobalEvent => Outcome[A]

  def present(
      context: UiContext,
      model: A
  ): Outcome[ComponentFragment]

  def reflow(model: A): A
