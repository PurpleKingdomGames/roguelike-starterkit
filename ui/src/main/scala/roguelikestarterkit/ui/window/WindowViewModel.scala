package roguelikestarterkit.ui.window

import indigo.*
import indigo.syntax.*
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UIContext

final case class WindowViewModel[ReferenceData](
    id: WindowId,
    modelHashCode: Int,
    contentRectangle: Bounds,
    dragData: Option[DragData],
    resizeData: Option[DragData],
    mouseIsOver: Boolean,
    magnification: Int
):

  def update[A](
      context: UIContext[ReferenceData],
      model: WindowModel[A, ReferenceData],
      event: GlobalEvent
  ): Outcome[WindowViewModel[ReferenceData]] =
    WindowViewModel.updateViewModel(context, model, this)(event)

  def resize[A](model: WindowModel[A, ReferenceData]): WindowViewModel[ReferenceData] =
    this // TODO
    // this.copy(terminal = WindowViewModel.makeWindowTerminal(model, terminal))

object WindowViewModel:

  def initial[ReferenceData](id: WindowId, magnification: Int): WindowViewModel[ReferenceData] =
    WindowViewModel(
      id,
      0,
      Bounds.zero,
      None,
      None,
      false,
      magnification
    )

  def updateViewModel[A, ReferenceData](
      context: UIContext[ReferenceData],
      model: WindowModel[A, ReferenceData],
      viewModel: WindowViewModel[ReferenceData]
  ): GlobalEvent => Outcome[WindowViewModel[ReferenceData]] =
    case FrameTick
        if model.bounds.hashCode() != viewModel.modelHashCode ||
          viewModel.dragData.isDefined ||
          viewModel.resizeData.isDefined =>
      Outcome(redraw(context, model, viewModel))

    case WindowInternalEvent.Redraw =>
      Outcome(redraw(context, model, viewModel))

    case WindowInternalEvent.ClearData =>
      Outcome(viewModel.copy(resizeData = None, dragData = None))

    case e: MouseEvent.Click =>
      val gridPos = context.mouseCoords

      val actionsAllowed = viewModel.dragData.isEmpty && viewModel.resizeData.isEmpty

      val close =
        if actionsAllowed && model.closeable && gridPos == model.bounds.topRight + Coords(-1, 0)
        then Batch(WindowEvent.Close(model.id))
        else Batch.empty

      Outcome(viewModel)
        .addGlobalEvents(close)

    case e: MouseEvent.MouseDown
        if context.isActive && model.draggable &&
          viewModel.dragData.isEmpty &&
          model.bounds.withDimensions(model.bounds.width, 3).contains(context.mouseCoords) &&
          context.mouseCoords != model.bounds.topRight + Coords(-1, 0) =>
      val d = calculateDragBy(model.snapGrid, e.position, model.bounds.coords)

      Outcome(viewModel.copy(dragData = Option(DragData(d, d))))

    case e: MouseEvent.MouseDown
        if context.isActive && model.resizable &&
          viewModel.resizeData.isEmpty &&
          model.bounds.bottomRight - Coords(1) == (context.mouseCoords) =>
      val d = calculateDragBy(model.snapGrid, e.position, model.bounds.coords)

      Outcome(viewModel.copy(resizeData = Option(DragData(d, d))))

    case e: MouseEvent.MouseUp if viewModel.dragData.isDefined =>
      Outcome(viewModel)
        .addGlobalEvents(
          WindowInternalEvent.MoveBy(
            model.id,
            viewModel.dragData
              .map(
                _.copy(by = calculateDragBy(model.snapGrid, e.position, model.bounds.coords))
              )
              .getOrElse(DragData.zero)
          ),
          WindowInternalEvent.ClearData
        )

    case e: MouseEvent.MouseUp if viewModel.resizeData.isDefined =>
      Outcome(viewModel)
        .addGlobalEvents(
          WindowInternalEvent.ResizeBy(
            model.id,
            viewModel.resizeData
              .map(
                _.copy(by = calculateDragBy(model.snapGrid, e.position, model.bounds.coords))
              )
              .getOrElse(DragData.zero)
          ),
          WindowInternalEvent.ClearData
        )

    case e: MouseEvent.Move if viewModel.dragData.isDefined =>
      Outcome(
        viewModel.copy(
          dragData = viewModel.dragData.map(
            _.copy(by = calculateDragBy(model.snapGrid, e.position, model.bounds.coords))
          )
        )
      )

    case e: MouseEvent.Move if viewModel.resizeData.isDefined =>
      Outcome(
        viewModel.copy(
          resizeData = viewModel.resizeData.map(
            _.copy(by = calculateDragBy(model.snapGrid, e.position, model.bounds.coords))
          )
        )
      )

    case MouseEvent.Move(pt)
        if viewModel.mouseIsOver && !model.bounds
          .toScreenSpace(context.snapGrid)
          .contains(pt) =>
      Outcome(viewModel.copy(mouseIsOver = false))
        .addGlobalEvents(WindowEvent.MouseOut(model.id))

    case MouseEvent.Move(pt)
        if !viewModel.mouseIsOver && model.bounds
          .toScreenSpace(context.snapGrid)
          .contains(pt) =>
      Outcome(viewModel.copy(mouseIsOver = true))
        .addGlobalEvents(WindowEvent.MouseOver(model.id))

    case _ =>
      Outcome(viewModel)

  def calculateDragBy(charSize: Size, mousePosition: Point, windowPosition: Coords): Coords =
    Coords(mousePosition / charSize.toPoint) - windowPosition

  def redraw[A, ReferenceData](
      context: UIContext[ReferenceData],
      model: WindowModel[A, ReferenceData],
      viewModel: WindowViewModel[ReferenceData]
  ): WindowViewModel[ReferenceData] =
    val tempModel =
      model
        .withDimensions(
          model.bounds.dimensions + viewModel.resizeData
            .map(d => d.by - d.offset)
            .getOrElse(Coords.zero)
            .toDimensions
        )
        .moveBy(
          viewModel.dragData
            .map(d => d.by - d.offset)
            .getOrElse(Coords.zero)
        )

    val vm = viewModel.resize(tempModel)
    // val clones =
    //   vm.terminal.toCloneTiles(
    //     CloneId("window_tile"),
    //     tempModel.bounds.coords.toScreenSpace(model.snapGrid),
    //     model.charSheet.charCrops
    //   ) { case (fg, bg) =>
    //     graphic.withMaterial(TerminalMaterial(model.charSheet.assetName, fg, bg))
    //   }

    val contentRectangle =
      WindowView.calculateContentRectangle(tempModel.bounds, model)

    vm.copy(
      // terminalClones = clones,
      modelHashCode = model.bounds.hashCode(),
      contentRectangle = contentRectangle
    )

  // def makeWindowTerminal[A, ReferenceData](
  //     model: WindowModel[A, ReferenceData],
  //     current: RogueTerminalEmulator
  // ): RogueTerminalEmulator =
  //   val validSize =
  //     model.bounds.dimensions.max(if model.title.isDefined then Dimensions(3) else Dimensions(2))

  //   val tiles: Batch[(Point, MapTile)] =
  //     val grey  = RGBA.White.mix(RGBA.Black, if model.hasFocus then 0.4 else 0.8)
  //     val title = model.title.getOrElse("").take(model.bounds.dimensions.width - 2).toCharArray()

  //     (0 to validSize.height).toBatch.flatMap { _y =>
  //       (0 to validSize.width).toBatch.map { _x =>
  //         val maxX   = validSize.width - 1
  //         val maxY   = validSize.height - 1
  //         val coords = Point(_x, _y)

  //         coords match
  //           // When there is a title
  //           case Point(0, 1) if model.title.isDefined =>
  //             // Title bar left
  //             coords -> MapTile(Tile.`│`, RGBA.White, RGBA.Black)

  //           case Point(x, 1) if model.title.isDefined && x == maxX =>
  //             // Title bar right
  //             coords -> MapTile(Tile.`│`, RGBA.White, RGBA.Black)

  //           case Point(x, 1) if model.title.isDefined =>
  //             // Title text, x starts at 2
  //             val idx = x - 1
  //             val tile =
  //               if idx >= 0 && idx < title.length then
  //                 val c = title(idx)
  //                 Tile.charCodes.get(if c == '\\' then "\\" else c.toString) match
  //                   case None       => Tile.SPACE
  //                   case Some(char) => Tile(char)
  //               else Tile.SPACE

  //             coords -> MapTile(tile, RGBA.White, RGBA.Black)

  //           case Point(0, 2) if model.title.isDefined =>
  //             // Title bar line left
  //             val tile = if maxY > 2 then Tile.`├` else Tile.`└`
  //             coords -> MapTile(Tile.`│`, RGBA.White, RGBA.Black)

  //           case Point(x, 2) if model.title.isDefined && x == maxX =>
  //             // Title bar line right
  //             val tile = if maxY > 2 then Tile.`┤` else Tile.`┘`
  //             coords -> MapTile(tile, RGBA.White, RGBA.Black)

  //           case Point(x, 2) if model.title.isDefined =>
  //             // Title bar line
  //             coords -> MapTile(Tile.`─`, RGBA.White, RGBA.Black)

  //           // Normal window frame

  //           case Point(0, 0) =>
  //             // top left
  //             coords -> MapTile(Tile.`┌`, RGBA.White, RGBA.Black)

  //           case Point(x, 0) if model.closeable && x == maxX =>
  //             // top right closable
  //             coords -> MapTile(Tile.`x`, RGBA.Black, RGBA.White)

  //           case Point(x, 0) if x == maxX =>
  //             // top right
  //             coords -> MapTile(Tile.`┐`, RGBA.White, RGBA.Black)

  //           case Point(x, 0) =>
  //             // top
  //             coords -> MapTile(Tile.`─`, RGBA.White, RGBA.Black)

  //           case Point(0, y) if y == maxY =>
  //             // bottom left
  //             coords -> MapTile(Tile.`└`, RGBA.White, RGBA.Black)

  //           case Point(x, y) if model.resizable && x == maxX && y == maxY =>
  //             // bottom right with resize
  //             coords -> MapTile(Tile.`▼`, RGBA.White, RGBA.Black)

  //           case Point(x, y) if x == maxX && y == maxY =>
  //             // bottom right
  //             coords -> MapTile(Tile.`┘`, RGBA.White, RGBA.Black)

  //           case Point(x, y) if y == maxY =>
  //             // bottom
  //             coords -> MapTile(Tile.`─`, RGBA.White, RGBA.Black)

  //           case Point(0, y) =>
  //             // Middle left
  //             coords -> MapTile(Tile.`│`, RGBA.White, RGBA.Black)

  //           case Point(x, y) if x == maxX =>
  //             // Middle right
  //             coords -> MapTile(Tile.`│`, RGBA.White, RGBA.Black)

  //           case Point(x, y) =>
  //             // Window background
  //             coords -> MapTile(Tile.`░`, grey, RGBA.Black)

  //       }
  //     }

  //   RogueTerminalEmulator(validSize.unsafeToSize)
  //     .put(tiles)
