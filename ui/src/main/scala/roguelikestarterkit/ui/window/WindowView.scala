package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.component.Component
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Bounds.dimensions
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UIContext
import roguelikestarterkit.ui.shaders.LayerMask

object WindowView:

  def present[A, ReferenceData](
      context: UIContext[ReferenceData],
      model: Window[A, ReferenceData],
      viewModel: WindowViewModel[ReferenceData]
  ): Outcome[Layer] =
    model.component
      .present(
        context.copy(bounds = viewModel.contentRectangle),
        model.content
      )
      .map(_.toLayer)
      .flatMap {
        case l: Layer.Content =>
          model.present(context, model).map { windowChrome =>
            Layer.Stack(
              windowChrome,
              l.withBlendMaterial(
                LayerMask(
                  viewModel.contentRectangle
                    .toScreenSpace(context.snapGrid * viewModel.magnification)
                )
              )
            )
          }

        case l: Layer.Stack =>
          val masked =
            l.layers.map {
              case l: Layer.Content =>
                l.withBlendMaterial(
                  LayerMask(
                    viewModel.contentRectangle
                      .toScreenSpace(context.snapGrid * viewModel.magnification)
                  )
                )

              case l =>
                l
            }

          model.present(context, model).map { windowChrome =>
            Layer.Stack(
              windowChrome :: masked
            )
          }
      }

  // TODO: This is now wrong.
  def calculateContentRectangle[A, ReferenceData](
      workingBounds: Bounds,
      model: Window[A, ReferenceData]
  ): Bounds =
    workingBounds
      .resize((workingBounds.dimensions - Dimensions(2, 2)).max(Dimensions.zero))
      .moveTo(workingBounds.coords + Coords(1, 1))
