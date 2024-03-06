package roguelikestarterkit.ui.window

import indigo.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UiContext

final case class WindowManagerModel[StartupData, A](windows: Batch[WindowModel[StartupData, A, _]]):
  def add(windowModels: WindowModel[StartupData, A, _]*): WindowManagerModel[StartupData, A] =
    add(Batch.fromSeq(windowModels))
  def add(windowModels: Batch[WindowModel[StartupData, A, _]]): WindowManagerModel[StartupData, A] =
    this.copy(windows = windows ++ windowModels)

  def remove(id: WindowId): WindowManagerModel[StartupData, A] =
    this.copy(windows = windows.filterNot(_.id == id))

  def giveFocusAndSurfaceAt(coords: Coords): WindowManagerModel[StartupData, A] =
    val reordered =
      windows.reverse.find(w => !w.static && w.bounds.contains(coords)) match
        case None =>
          windows

        case Some(w) =>
          windows.filterNot(_.id == w.id).map(_.blur) :+ w.focus

    this.copy(windows = reordered)

  def moveTo(id: WindowId, position: Coords): WindowManagerModel[StartupData, A] =
    this.copy(windows = windows.map(w => if w.id == id then w.moveTo(position) else w))

  def resizeTo(id: WindowId, dimensions: Dimensions): WindowManagerModel[StartupData, A] =
    this.copy(windows = windows.map(w => if w.id == id then w.resizeTo(dimensions) else w))

  def transformTo(id: WindowId, bounds: Bounds): WindowManagerModel[StartupData, A] =
    this.copy(windows =
      windows.map(w =>
        // Note: We do _not_ use .withBounds here because that won't do the min size checks.
        if w.id == id then w.moveTo(bounds.coords).resizeTo(bounds.dimensions) else w
      )
    )

  def update(
      context: UiContext[StartupData, A],
      event: GlobalEvent
  ): Outcome[WindowManagerModel[StartupData, A]] =
    WindowManager.updateModel(context, this)(event)

object WindowManagerModel:
  def initial[StartupData, A]: WindowManagerModel[StartupData, A] =
    WindowManagerModel(Batch.empty)
