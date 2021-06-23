package roguelike

import indigo._

object Assets:

  val charsTransparentName = AssetName("Anikki_square_10x10_transparent")
  val charsName            = AssetName("Anikki_square_10x10")

  val fontMaterial: Material.ImageEffects = Material.ImageEffects(charsTransparentName)

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(charsTransparentName, AssetPath("assets/" + charsTransparentName.toString + ".png")),
      AssetType.Image(charsName, AssetPath("assets/" + charsName.toString + ".png"))
    )
