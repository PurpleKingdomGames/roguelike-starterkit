package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.UIContext

object WindowView:

  def present[A, ReferenceData](
      context: UIContext[ReferenceData],
      model: Window[A, ReferenceData],
      viewModel: WindowViewModel[ReferenceData]
  ): Outcome[Layer] =
    model.component
      .present(
        context.copy(bounds = model.bounds),
        model.content
      )
      .flatMap {
        case l: Layer.Content =>
          model.background(WindowContext.from(model, viewModel)).map { windowChrome =>
            Layer.Stack(
              windowChrome,
              l
            )
          }

        case l: Layer.Stack =>
          model.background(WindowContext.from(model, viewModel)).map { windowChrome =>
            l.prepend(windowChrome)
          }
      }
