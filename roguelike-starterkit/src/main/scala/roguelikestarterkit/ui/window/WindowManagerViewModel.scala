package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.UiContext

final case class WindowManagerViewModel[A](windows: Batch[WindowViewModel]):
  def prune(model: WindowManagerModel[A]): WindowManagerViewModel[A] =
    this.copy(windows = windows.filter(w => model.windows.exists(_.id == w.id)))

  def update(
      context: UiContext,
      model: WindowManagerModel[A],
      event: GlobalEvent
  ): Outcome[WindowManagerViewModel[A]] =
    WindowManager.updateViewModel(context, model, this)(event)

  def mouseIsOverAnyWindow: Boolean =
    windows.exists(_.mouseIsOver)

  def mouseIsOver: Batch[WindowId] =
    windows.collect { case wvm if wvm.mouseIsOver => wvm.id }

object WindowManagerViewModel:
  def initial[A]: WindowManagerViewModel[A] =
    WindowManagerViewModel(Batch.empty)
