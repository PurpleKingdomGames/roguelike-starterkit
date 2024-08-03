package roguelikestarterkit.ui.components.datatypes

final case class ScrollOptions(
    scrollMode: ScrollMode,
    scrollSpeed: Int
):

  def withScrollMode(scrollMode: ScrollMode): ScrollOptions =
    copy(scrollMode = scrollMode)

  def withScrollSpeed(scrollSpeed: Int): ScrollOptions =
    copy(scrollSpeed = scrollSpeed)

  def isEnabled: Boolean =
    scrollMode != ScrollMode.None

  def isDisabled: Boolean =
    scrollMode == ScrollMode.None

object ScrollOptions:
  val default: ScrollOptions = ScrollOptions(ScrollMode.Vertical, 4)

enum ScrollMode:
  case None
  case Vertical
