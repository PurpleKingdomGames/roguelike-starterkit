package roguelike

import indigo._
import indigo.scenes._

object GameScene extends Scene[Unit, Unit, Unit] {

  type SceneModel     = Unit
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("empty")

  val modelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val viewModelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: FrameContext[Unit], model: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Unit,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  val size = Size(30)

  def present(context: FrameContext[Unit], model: Unit, viewModel: Unit): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Text("░░░░░\n░░@░░\n░░░░░", DfTiles.Fonts.fontKey, Assets.fontMaterial(RGBA.Magenta)),
        Text("░░░\n░@░\n░░░", DfTiles.Fonts.fontKey, Assets.fontMaterial(RGBA.Cyan)).moveBy(0, 50),
        MapRenderer(Point(100), size, Depth(1))
      )
    )

}
