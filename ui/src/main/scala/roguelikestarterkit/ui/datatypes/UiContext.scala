package roguelikestarterkit.ui.datatypes

import indigo.*

final case class UIContext[ReferenceData](
    // Specific to UIContext
    bounds: Bounds,
    snapGrid: Size,
    mouseCoords: Coords,
    state: UIState,
    magnification: Int,
    additionalOffset: Coords,
    // The following are all the same as in SubSystemContext
    reference: ReferenceData,
    frame: Context.Frame,
    services: Context.Services
):
  lazy val screenSpaceBounds: Rectangle =
    bounds.toScreenSpace(snapGrid)

  def moveBoundsBy(offset: Coords): UIContext[ReferenceData] =
    this.copy(bounds = bounds.moveBy(offset))

  val isActive: Boolean =
    state == UIState.Active

  def unitReference: UIContext[Unit] =
    this.copy(reference = ())

  def withAdditionalOffset(offset: Coords): UIContext[ReferenceData] =
    this.copy(additionalOffset = offset)

object UIContext:

  def apply[ReferenceData](
      subSystemContext: SubSystemContext[ReferenceData],
      snapGrid: Size,
      magnification: Int
  ): UIContext[ReferenceData] =
    val mouseCoords = Coords(subSystemContext.frame.input.mouse.position / snapGrid.toPoint)
    UIContext(
      Bounds.zero,
      snapGrid,
      mouseCoords,
      UIState.Active,
      magnification,
      Coords.zero,
      subSystemContext.reference,
      subSystemContext.frame,
      subSystemContext.services
    )

  def apply[ReferenceData](
      subSystemContext: SubSystemContext[ReferenceData],
      snapGrid: Size,
      magnification: Int,
      additionalOffset: Coords
  ): UIContext[ReferenceData] =
    val mouseCoords = Coords(subSystemContext.frame.input.mouse.position / snapGrid.toPoint)
    UIContext(
      Bounds.zero,
      snapGrid,
      mouseCoords,
      UIState.Active,
      magnification,
      additionalOffset,
      subSystemContext.reference,
      subSystemContext.frame,
      subSystemContext.services
    )

enum UIState:
  case Active, InActive

  def isActive: Boolean =
    this match
      case UIState.Active   => true
      case UIState.InActive => false

  def isInActive: Boolean =
    !isActive
