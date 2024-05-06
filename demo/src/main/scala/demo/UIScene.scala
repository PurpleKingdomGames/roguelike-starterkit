package demo

import indigo.*
import indigo.scenes.*
import roguelikestarterkit.*

object UIScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("UI scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem[Model]] =
    Set(
      WindowManager[Model, Unit](
        SubSystemId("window manager 2"),
        RogueLikeGame.magnification,
        Model.defaultCharSheet,
        _ => ()
      )
        .register(
          ComponentsWindow.window(
            Model.defaultCharSheet
          )
        )
        .open(ComponentsWindow.windowId)
    )

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
    Outcome(
      SceneUpdateFragment.empty
    )
