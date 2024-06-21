package demo

import indigo.*
import indigo.scenes.*
import indigo.shared.subsystems.SubSystemFrameContext.*
import roguelikestarterkit.*

object NoTerminalUI extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("NoTerminalUI scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem[Model]] =
    Set()

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    val label =
      Label[Int]("Custom rendered label", (_, t) => Bounds(0, 0, 150, 20)) {
        case (offset, label, dimensions) =>
          Outcome(
            ComponentFragment(
              TextBox(
                label,
                dimensions.width,
                dimensions.height
              ).moveTo(offset.x, offset.y)
                .withColor(RGBA.Red)
            )
          )
      }

    val rendered =
      summon[StatelessComponent[Label[Int], Int]]
        .present(UIContext(context.frameContext.forSubSystems.copy(reference = 0), Size(1)), label)

    rendered.map { componentFragment =>
      SceneUpdateFragment(
        componentFragment.toLayer
      )
    }
