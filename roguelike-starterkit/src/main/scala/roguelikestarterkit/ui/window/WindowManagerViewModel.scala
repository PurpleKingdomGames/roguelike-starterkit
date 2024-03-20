package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.UiContext

final case class WindowManagerViewModel(windows: Batch[WindowViewModel], magnification: Int):
  def prune(model: WindowManagerModel): WindowManagerViewModel =
    this.copy(windows = windows.filter(w => model.windows.exists(_.id == w.id)))

  def update(
      context: UiContext,
      model: WindowManagerModel,
      event: GlobalEvent
  ): Outcome[WindowManagerViewModel] =
    WindowManager.updateViewModel(context, model, this)(event)

  def mouseIsOverAnyWindow: Boolean =
    windows.exists(_.mouseIsOver)

  def mouseIsOver: Batch[WindowId] =
    windows.collect { case wvm if wvm.mouseIsOver => wvm.id }

  def changeMagnification(next: Int): WindowManagerViewModel =
    this.copy(
      windows = windows.map(_.copy(magnification = next)),
      magnification = next
    )

object WindowManagerViewModel:
  def initial[A](magnification: Int): WindowManagerViewModel =
    WindowManagerViewModel(Batch.empty, magnification)
