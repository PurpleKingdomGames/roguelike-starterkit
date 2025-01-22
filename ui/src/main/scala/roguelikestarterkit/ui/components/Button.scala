package roguelikestarterkit.ui.components

import indigo.*
import indigo.syntax.*
import roguelikestarterkit.ui.component.Component
import roguelikestarterkit.ui.component.ComponentFragment
import roguelikestarterkit.ui.datatypes.Bounds
import roguelikestarterkit.ui.datatypes.Coords
import roguelikestarterkit.ui.datatypes.Dimensions
import roguelikestarterkit.ui.datatypes.UIContext

/** Buttons `Component`s allow you to create buttons for your UI.
  */
final case class Button[ReferenceData](
    bounds: Bounds,
    state: ButtonState,
    up: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment],
    over: Option[(Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]],
    down: Option[(Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]],
    click: ReferenceData => Batch[GlobalEvent],
    press: ReferenceData => Batch[GlobalEvent],
    release: ReferenceData => Batch[GlobalEvent],
    drag: (ReferenceData, DragData) => Batch[GlobalEvent],
    boundsType: BoundsType[ReferenceData, Unit],
    isDown: Boolean,
    dragOptions: DragOptions,
    dragStart: Option[DragData]
):
  val isDragged: Boolean = dragStart.isDefined

  def presentUp(
      up: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    this.copy(up = up)

  def presentOver(
      over: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    this.copy(over = Option(over))

  def presentDown(
      down: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    this.copy(down = Option(down))

  def onClick(events: ReferenceData => Batch[GlobalEvent]): Button[ReferenceData] =
    this.copy(click = events)
  def onClick(events: Batch[GlobalEvent]): Button[ReferenceData] =
    onClick(_ => events)
  def onClick(events: GlobalEvent*): Button[ReferenceData] =
    onClick(Batch.fromSeq(events))

  def onPress(events: ReferenceData => Batch[GlobalEvent]): Button[ReferenceData] =
    this.copy(press = events)
  def onPress(events: Batch[GlobalEvent]): Button[ReferenceData] =
    onPress(_ => events)
  def onPress(events: GlobalEvent*): Button[ReferenceData] =
    onPress(Batch.fromSeq(events))

  def onRelease(events: ReferenceData => Batch[GlobalEvent]): Button[ReferenceData] =
    this.copy(release = events)
  def onRelease(events: Batch[GlobalEvent]): Button[ReferenceData] =
    onRelease(_ => events)
  def onRelease(events: GlobalEvent*): Button[ReferenceData] =
    onRelease(Batch.fromSeq(events))

  /** Drag events are fired when the mouse is pressed on the button and then moved without release.
    * The Coords argument to the function are RELATIVE to the button's position, i.e. dragging up
    * and left will result in negative coordinates.
    */
  def onDrag(
      events: (ReferenceData, DragData) => Batch[GlobalEvent]
  ): Button[ReferenceData] =
    this.copy(drag = events)
  def onDrag(events: Batch[GlobalEvent]): Button[ReferenceData] =
    onDrag((_, _) => events)
  def onDrag(events: GlobalEvent*): Button[ReferenceData] =
    onDrag(Batch.fromSeq(events))

  def withDragOptions(value: DragOptions): Button[ReferenceData] =
    this.copy(dragOptions = value)
  def makeDraggable: Button[ReferenceData] =
    withDragOptions(DragOptions.Drag)
  def reportDrag: Button[ReferenceData] =
    withDragOptions(DragOptions.ReportDrag)
  def notDraggable: Button[ReferenceData] =
    withDragOptions(DragOptions.None)

  def withBoundsType(value: BoundsType[ReferenceData, Unit]): Button[ReferenceData] =
    this.copy(boundsType = value)

object Button:

  /** Minimal button constructor with custom rendering function
    */
  def apply[ReferenceData](boundsType: BoundsType[ReferenceData, Unit])(
      present: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    Button(
      Bounds.zero,
      ButtonState.Up,
      present,
      None,
      None,
      _ => Batch.empty,
      _ => Batch.empty,
      _ => Batch.empty,
      (_, _) => Batch.empty,
      boundsType,
      isDown = false,
      dragOptions = DragOptions.None,
      dragStart = None
    )

  /** Minimal button constructor with custom rendering function
    */
  def apply[ReferenceData](bounds: Bounds)(
      present: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    Button(
      bounds,
      ButtonState.Up,
      present,
      None,
      None,
      _ => Batch.empty,
      _ => Batch.empty,
      _ => Batch.empty,
      (_, _) => Batch.empty,
      BoundsType.Fixed(bounds),
      isDown = false,
      dragOptions = DragOptions.None,
      dragStart = None
    )

  /** Minimal button constructor with custom rendering function and dynamic sizing
    */
  def apply[ReferenceData](calculateBounds: ReferenceData => Bounds)(
      present: (Coords, Bounds, ReferenceData) => Outcome[ComponentFragment]
  ): Button[ReferenceData] =
    Button(
      Bounds.zero,
      ButtonState.Up,
      present,
      None,
      None,
      _ => Batch.empty,
      _ => Batch.empty,
      _ => Batch.empty,
      (_, _) => Batch.empty,
      BoundsType.Calculated(calculateBounds),
      isDown = false,
      dragOptions = DragOptions.None,
      dragStart = None
    )

  given [ReferenceData]: Component[Button[ReferenceData], ReferenceData] with
    def bounds(reference: ReferenceData, model: Button[ReferenceData]): Bounds =
      model.bounds

    def updateModel(
        context: UIContext[ReferenceData],
        model: Button[ReferenceData]
    ): GlobalEvent => Outcome[Button[ReferenceData]] =
      case FrameTick =>
        val newBounds =
          model.boundsType match
            case BoundsType.Fixed(bounds) =>
              bounds

            case BoundsType.Calculated(calculate) =>
              calculate(context.reference, ())

            case _ =>
              model.bounds

        def decideState: ButtonState =
          if model.isDown then ButtonState.Down
          else if newBounds.moveBy(context.bounds.coords).contains(context.mouseCoords) then
            if context.mouse.isLeftDown then ButtonState.Down
            else ButtonState.Over
          else ButtonState.Up

        Outcome(
          model.copy(
            state =
              if context.isActive then decideState
              else ButtonState.Up,
            bounds = newBounds
          )
        )

      case _: MouseEvent.Click
          if context.isActive && model.bounds
            .moveBy(context.bounds.coords)
            .contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))
          .addGlobalEvents(model.click(context.reference))

      case _: MouseEvent.MouseDown
          if context.isActive && model.bounds
            .moveBy(context.bounds.coords)
            .contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Down, isDown = true, dragStart = None))
          .addGlobalEvents(model.press(context.reference))

      case _: MouseEvent.MouseUp
          if context.isActive && model.bounds
            .moveBy(context.bounds.coords)
            .contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))
          .addGlobalEvents(model.release(context.reference))

      case _: MouseEvent.MouseUp =>
        // Released Outside.
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))

      case _: MouseEvent.Move
          if context.isActive && model.isDown && model.dragOptions.isDraggable =>
        def makeDragData =
          DragData(
            start = context.mouseCoords,
            position = context.mouseCoords,
            offset = context.mouseCoords - context.bounds.coords,
            delta = Coords.zero
          )

        val newDragStart =
          if model.dragStart.isEmpty then makeDragData
          else model.dragStart.getOrElse(makeDragData)

        val updatedDragData =
          newDragStart.copy(
            position = context.mouseCoords,
            delta = context.mouseCoords - newDragStart.start
          )

        Outcome(
          model.copy(dragStart = Option(updatedDragData))
        ).addGlobalEvents(
          model.drag(context.reference, updatedDragData)
        )

      case _ =>
        Outcome(model)

    def present(
        context: UIContext[ReferenceData],
        model: Button[ReferenceData]
    ): Outcome[ComponentFragment] =
      val b =
        if model.isDragged && model.dragOptions.followMouse then
          model.bounds.moveBy(
            model.dragStart.map(dd => context.mouseCoords - dd.start).getOrElse(Coords.zero)
          )
        else model.bounds

      model.state match
        case ButtonState.Up =>
          model
            .up(context.bounds.coords, b, context.reference)

        case ButtonState.Over =>
          model.over
            .getOrElse(model.up)(context.bounds.coords, b, context.reference)

        case ButtonState.Down =>
          model.down
            .getOrElse(model.up)(context.bounds.coords, b, context.reference)

    def refresh(
        reference: ReferenceData,
        model: Button[ReferenceData],
        parentDimensions: Dimensions
    ): Button[ReferenceData] =
      model.boundsType match
        case BoundsType.Fixed(bounds) =>
          model.copy(
            bounds = bounds
          )

        case BoundsType.Calculated(calculate) =>
          model

        case BoundsType.FillWidth(height, padding) =>
          model.copy(
            bounds = Bounds(
              parentDimensions.width - padding.left - padding.right,
              height
            )
          )

        case BoundsType.FillHeight(width, padding) =>
          model.copy(
            bounds = Bounds(
              width,
              parentDimensions.height - padding.top - padding.bottom
            )
          )

        case BoundsType.Fill(padding) =>
          model.copy(
            bounds = Bounds(
              parentDimensions.width - padding.left - padding.right,
              parentDimensions.height - padding.top - padding.bottom
            )
          )

enum ButtonState:
  case Up, Over, Down

  def isUp: Boolean =
    this match
      case Up => true
      case _  => false

  def is: Boolean =
    this match
      case Over => true
      case _    => false

  def isDown: Boolean =
    this match
      case Down => true
      case _    => false

/** Describes the drag behaviour of the component
  */
enum DragOptions:

  /** Cannot be dragged
    */
  case None

  /** The drag movement is tracked and reported (via events), but the component is rendered as
    * normal, i.e. does not move.
    */
  case ReportDrag

  /** The component follows the mouse movement as well as emitting relevant events.
    */
  case Drag

  def isDraggable: Boolean =
    this match
      case None       => false
      case ReportDrag => true
      case Drag       => true

  def followMouse: Boolean =
    this match
      case None       => false
      case ReportDrag => false
      case Drag       => true

/** Data about the ongoing drag operation, all positions are in screen space.
  *
  * @param start
  *   The position of the mouse when the drag started
  * @param position
  *   The current position of the mouse
  * @param offset
  *   The start position relative to the component
  * @param delta
  *   The change in position since the drag started
  */
final case class DragData(
    start: Coords,
    position: Coords,
    offset: Coords,
    delta: Coords
)
