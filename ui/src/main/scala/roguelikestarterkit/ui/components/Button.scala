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
    calculateBounds: ReferenceData => Bounds,
    isDown: Boolean
):
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
      _ => bounds,
      isDown = false
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
      calculateBounds,
      isDown = false
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
        Outcome(model.copy(state = ButtonState.Up, isDown = false))
          .addGlobalEvents(model.click(context.reference))

      case _: MouseEvent.MouseDown
          if model.bounds.moveBy(context.bounds.coords).contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Down, isDown = true))

      case _: MouseEvent.MouseUp
          if model.bounds.moveBy(context.bounds.coords).contains(context.mouseCoords) =>
        Outcome(model.copy(state = ButtonState.Up, isDown = false))

      case _: MouseEvent.MouseUp =>
        // Released Outside.
        Outcome(model.copy(state = ButtonState.Up, isDown = false))

      case _ =>
        Outcome(model)

    def present(
        context: UIContext[ReferenceData],
        model: Button[ReferenceData]
    ): Outcome[ComponentFragment] =
      val b           = model.bounds
      val mouseWithin = b.moveBy(context.bounds.coords).contains(context.mouseCoords)

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
