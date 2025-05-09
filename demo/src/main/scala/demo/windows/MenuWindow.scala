package demo.windows

import demo.models.Log
import indigo.*
import indigoextras.ui.*
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*
import roguelikestarterkit.ui.*

object MenuWindow:

  val windowId: WindowId = WindowId("MenuWindow")

  def window(
      charSheet: CharSheet
  ): Window[ComponentList[Int], Int] =
    TerminalWindow(
      windowId,
      charSheet,
      ComponentList(Dimensions(20, 3)) { (_: UIContext[Int]) =>
        Batch(
          ComponentId("btn1") ->
            TerminalButton[Int](
              "Window 1",
              TerminalButton.Theme(
                charSheet,
                RGBA.Silver -> RGBA.Black,
                RGBA.White  -> RGBA.Black,
                RGBA.Black  -> RGBA.White,
                hasBorder = false
              )
            ).onClick(Log("Window 1"), WindowEvent.Open(ComponentsWindow.windowId)),
          ComponentId("btn2") ->
            TerminalButton[Int](
              "Window 2",
              TerminalButton.Theme(
                charSheet,
                RGBA.Silver -> RGBA.Black,
                RGBA.Green  -> RGBA.Black,
                RGBA.Black  -> RGBA.Yellow,
                hasBorder = false
              )
            ).onClick(Log("Window 2"), WindowEvent.Open(ComponentsWindow2.windowId))
        )
      }
        .withLayout(ComponentLayout.Horizontal(Padding(1, 0, 0, 1)))
    )
      .moveTo(0, 0)
      .resizeTo(20, 3)
