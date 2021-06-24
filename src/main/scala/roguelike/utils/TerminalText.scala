package roguelike.utils

import indigo.shared.assets.AssetName
import indigo.shared.materials.Material
import indigo.shared.materials.ShaderData
import indigo.shared.shader.ShaderPrimitive.vec4
import indigo.shared.datatypes.RGBA
import indigo.shared.shader.UniformBlock
import indigo.shared.shader.Uniform
import indigo.shared.shader.EntityShader
import indigo.shared.shader.ShaderId
import indigo.shared.datatypes.RGB

final case class TerminalText(
    tileMap: AssetName,
    foreground: RGBA,
    background: RGBA,
    mask: RGBA
) extends Material:

  def withForeground(newColor: RGBA): TerminalText =
    this.copy(foreground = newColor)
  def withForeground(newColor: RGB): TerminalText =
    this.copy(foreground = newColor.toRGBA)

  def withBackground(newColor: RGBA): TerminalText =
    this.copy(background = newColor)
  def withBackground(newColor: RGB): TerminalText =
    this.copy(background = newColor.toRGBA)

  def withMask(newColor: RGBA): TerminalText =
    this.copy(mask = newColor)
  def withMask(newColor: RGB): TerminalText =
    this.copy(mask = newColor.toRGBA)

  def toShaderData: ShaderData =
    ShaderData(
      TerminalText.shaderId,
      List(
        UniformBlock(
          "RogueLikeTextData",
          List(
            Uniform("FOREGROUND") -> vec4(foreground.r, foreground.g, foreground.b, foreground.a),
            Uniform("BACKGROUND") -> vec4(background.r, background.g, background.b, background.a),
            Uniform("MASK") -> vec4(mask.r, mask.g, mask.b, mask.a)
          )
        )
      ),
      Some(tileMap),
      None,
      None,
      None
    )

object TerminalText:

  val shaderId: ShaderId = ShaderId("roguelike text material")

  def shader(fragProgram: AssetName): EntityShader.External =
    EntityShader
      .External(shaderId)
      .withFragmentProgram(fragProgram)

  def apply(tileMap: AssetName): TerminalText =
    TerminalText(tileMap, RGBA.White, RGBA.Zero, RGBA.Magenta)

  def apply(tileMap: AssetName, color: RGBA): TerminalText =
    TerminalText(tileMap, color, RGBA.Zero, RGBA.Magenta)

  def apply(tileMap: AssetName, foreground: RGBA, background: RGBA): TerminalText =
    TerminalText(tileMap, foreground, background, RGBA.Magenta)
