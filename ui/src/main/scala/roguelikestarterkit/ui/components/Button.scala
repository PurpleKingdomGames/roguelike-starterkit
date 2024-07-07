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
    calculateBounds: ReferenceData => Bounds,
    isDown: Boolean,
    dragOptions: DragOptions,
    dragStart: Option[Coords]
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
  def onDrag(events: (ReferenceData, DragData) => Batch[GlobalEvent]): Button[ReferenceData] =
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

object Button:

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
      _ => bounds,
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
      calculateBounds,
      isDown = false,
      dragOptions = DragOptions.None,
      dragStart = None
    )

  given [ReferenceData]: Component[Button[ReferenceData], ReferenceData] with
    def bounds(reference: ReferenceData, model: Button[ReferenceData]): Bounds =
      model.calculateBounds(reference)

    def updateModel(
        context: UIContext[ReferenceData],
        model: Button[ReferenceData]
    ): GlobalEvent => Outcome[Button[ReferenceData]] =
      case FrameTick =>
        val newBounds =
          model.calculateBounds(context.reference)

        Outcome(
          model.copy(
            state =
              if model.isDown then ButtonState.Down
              else if newBounds.moveBy(context.bounds.coords).contains(context.mouseCoords) then
                if context.mouse.isLeftDown then ButtonState.Down
                else ButtonState.Over
              else ButtonState.Up,
            bounds = newBounds
          )
        )

      case _: MouseEvent.Click
          if model.bounds.moveBy(context.bounds.coords).contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))
          .addGlobalEvents(model.click(context.reference))

      case _: MouseEvent.MouseDown
          if model.bounds.moveBy(context.bounds.coords).contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Down, isDown = true, dragStart = None))
          .addGlobalEvents(model.press(context.reference))

      case _: MouseEvent.MouseUp
          if model.bounds.moveBy(context.bounds.coords).contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))
          .addGlobalEvents(model.release(context.reference))

      case _: MouseEvent.MouseUp =>
        // Released Outside.
        Outcome(model.copy(state = ButtonState.Up, isDown = false, dragStart = None))

      case _: MouseEvent.Move if model.isDown && model.dragOptions.isDraggable =>
        val newDragStart =
          if model.dragStart.isEmpty then Option(context.mouseCoords) else model.dragStart

        val start = newDragStart.getOrElse(Coords.zero)

        val dragData =
          DragData(
            start = start,
            current = context.mouseCoords,
            delta = context.mouseCoords - start
          )

        Outcome(
          model.copy(dragStart = newDragStart)
        ).addGlobalEvents(
          model.drag(context.reference, dragData)
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
            model.dragStart.map(pt => context.mouseCoords - pt).getOrElse(Coords.zero)
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
      model

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

final case class DragData(
    start: Coords,
    current: Coords,
    delta: Coords
)
