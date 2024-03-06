package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.UiContext

object WindowManager:

  def updateModel[StartupData, A](
      context: UiContext[StartupData, A],
      model: WindowManagerModel[StartupData, A]
  ): GlobalEvent => Outcome[WindowManagerModel[StartupData, A]] =
    case WindowManagerEvent.Close(id) =>
      Outcome(model.close(id))

    case WindowManagerEvent.GiveFocusAt(position) =>
      Outcome(model.giveFocusAndSurfaceAt(position))
        .addGlobalEvents(WindowEvent.Redraw)

    case WindowManagerEvent.Open(id) =>
      Outcome(model.open(id))

    case WindowManagerEvent.OpenAt(id, coords) =>
      Outcome(model.open(id).moveTo(id, coords))

    case WindowManagerEvent.Move(id, coords) =>
      Outcome(model.moveTo(id, coords))

    case WindowManagerEvent.Resize(id, dimensions) =>
      Outcome(model.resizeTo(id, dimensions))

    case WindowManagerEvent.Transform(id, bounds) =>
      Outcome(model.transformTo(id, bounds))

    case e =>
      model.windows
        .map(w => if w.isOpen then Window.updateModel(context, w)(e) else Outcome(w))
        .sequence
        .map(m => model.copy(windows = m))

  def updateViewModel[StartupData, A](
      context: UiContext[StartupData, A],
      model: WindowManagerModel[StartupData, A],
      viewModel: WindowManagerViewModel[StartupData, A]
  ): GlobalEvent => Outcome[WindowManagerViewModel[StartupData, A]] =
    case e =>
      val updated =
        val prunedVM = viewModel.prune(model)
        model.windows.flatMap { m =>
          if m.isClosed then Batch.empty
          else
            prunedVM.windows.find(_.id == m.id) match
              case None =>
                Batch(Outcome(WindowViewModel.initial(m.id)))

              case Some(vm) =>
                Batch(vm.update(context, m, e))
        }

      updated.sequence.map(vm => viewModel.copy(windows = vm))

  def present[StartupData, A](
      context: UiContext[StartupData, A],
      globalMagnification: Int,
      model: WindowManagerModel[StartupData, A],
      viewModel: WindowManagerViewModel[StartupData, A]
  ): Outcome[SceneUpdateFragment] =
    model.windows
      .filter(_.isOpen)
      .flatMap { m =>
        viewModel.windows.find(_.id == m.id) match
          case None =>
            // Shouldn't get here.
            Batch.empty

          case Some(vm) =>
            Batch(Window.present(context, globalMagnification, m, vm))
      }
      .sequence
      .map(
        _.foldLeft(SceneUpdateFragment.empty)(_ |+| _)
      )
