// package roguelikestarterkit.ui.window

// import indigo.*
// import roguelikestarterkit.ui.datatypes.UIContext

// final case class WindowManagerViewModel[ReferenceData](
//     windows: Batch[WindowViewModel[ReferenceData]],
//     magnification: Int
// ):
//   def prune(model: WindowManagerModel[ReferenceData]): WindowManagerViewModel[ReferenceData] =
//     this.copy(windows = windows.filter(w => model.windows.exists(_.id == w.id)))

//   def update(
//       context: UIContext[ReferenceData],
//       model: WindowManagerModel[ReferenceData],
//       event: GlobalEvent
//   ): Outcome[WindowManagerViewModel[ReferenceData]] =
//     WindowManager.updateViewModel(context, model, this)(event)

//   def mouseIsOverAnyWindow: Boolean =
//     windows.exists(_.mouseIsOver)

//   def mouseIsOver: Batch[WindowId] =
//     windows.collect { case wvm if wvm.mouseIsOver => wvm.id }

//   def changeMagnification(next: Int): WindowManagerViewModel[ReferenceData] =
//     this.copy(
//       windows = windows.map(_.copy(magnification = next)),
//       magnification = next
//     )

// object WindowManagerViewModel:
//   def initial[A, ReferenceData](magnification: Int): WindowManagerViewModel[ReferenceData] =
//     WindowManagerViewModel(Batch.empty, magnification)
