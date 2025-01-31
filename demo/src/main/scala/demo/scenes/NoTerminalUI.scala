package demo.scenes

import demo.models.ChangeValue
import demo.models.Model
import demo.models.ViewModel
import indigo.*
import indigo.scenes.*
import indigo.shared.subsystems.SubSystemContext.*
import indigoextras.ui.*
import indigoextras.ui.syntax.*

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
    case ChangeValue(value) =>
      Outcome(model.copy(num = value))

    case e =>
      val ctx =
        UIContext(context.toContext.forSubSystems.copy(reference = model.num), Size(1), 1)
      summon[Component[ComponentGroup[Int], Int]].updateModel(ctx, model.components)(e).map { cl =>
        model.copy(components = cl)
      }

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
    model.components
      .present(UIContext(context.toContext.forSubSystems.copy(reference = 0), Size(1), 1))
      .map(l => SceneUpdateFragment(l))
