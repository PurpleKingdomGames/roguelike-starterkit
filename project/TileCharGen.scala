import sbt._
import scala.sys.process._

object TileCharGen {

  def genFont(charWidth: Int, charHeight: Int, chars: List[CharDetail]): String = {
    val sheetWidth: Int  = charWidth * 16
    val sheetHeight: Int = charHeight * 16

    val charString = chars
      .map(cd => "       .addChar(" + cd.toFontChar(charWidth, charHeight) + ")")
      .mkString("\n")

    s"""  object Fonts {
    |
    |    val fontKey: FontKey = FontKey("DF-Roguelike Font ${charWidth.toString()}x${charHeight
      .toString()}")
    |
    |    val fontInfo: FontInfo =
    |      FontInfo(fontKey, $sheetWidth, $sheetHeight, FontChar(" ", 0, 0, ${charWidth.toString}, ${charHeight.toString})).isCaseSensitive
    |$charString
    |  }
    |""".stripMargin
  }

  def genTileCrops(charWidth: Int, charHeight: Int, chars: List[CharDetail]): String = {
    val crops = {
      val cs = chars
        .map(_.toCrop(charWidth, charHeight))
        .map { t =>
          s"""(${t._1.toString}, ${t._2.toString}, ${t._3.toString}, ${t._4.toString}),"""
        }
        .mkString("\n      ")

      s"""  val charCrops: Batch[(Int, Int, Int, Int)] = Batch(
      |    $cs
      |  )
      |""".stripMargin
    }

    s"""
    |$crops
    |""".stripMargin
  }

  def template(moduleName: String, fullyQualifiedPath: String, contents: String): String =
    s"""package $fullyQualifiedPath
    |
    |import indigo.*
    |
    |// GENERATED by TileCharGen.scala - DO NOT EDIT
    |object $moduleName {
    |
    |$contents
    |
    |}
    """.stripMargin

  def templateTile(fullyQualifiedPath: String, chars: List[CharDetail]): String = {
    val charString =
      chars
        .filterNot(cd => cd.index == 0 || cd.index == 255)
        .map { cd =>
          val c = cd.char match {
            case '\\' => None
            case '_'  => None
            case '\'' => None
            case '`'  => None
            case char => Some(char.toString)
          }

          s"""  ${c
            .map(cc => s"val `$cc`: Tile = ${cd.index.toString}")
            .getOrElse("// Reserved char")}
          |  val ${cd.name}: Tile = ${cd.index.toString}
          |""".stripMargin
        }
        .mkString("\n")

    val charToString: Char => String = c => if (c == '\\') "\\\\" else c.toString()

    val lookUp = {
      val cs = chars
        .filterNot(cd => cd.index == 0 || cd.index == 255)
        .map(cd => (s"""${charToString(cd.char)}""", cd.index))
        .map(p => s""""${p._1}" -> ${p._2.toString()},""")
        .dropRight(1) // remove last comma
        .mkString("\n      ")

      s"""  val charCodes: Map[String, Int] = Map(
      |    $cs
      |  )
      |""".stripMargin
    }

    s"""package $fullyQualifiedPath
    |
    |// GENERATED by TileCharGen.scala - DO NOT EDIT
    |opaque type Tile = Int
    |
    |object Tile {
    |  inline def apply(char: Int): Tile = char
    |    
    |  extension (t: Tile)
    |    def toInt: Int = t
    |    def toFloat: Float = t.toFloat
    |
    |$charString
    |
    |$lookUp
    |
    |}
    """.stripMargin
  }

  def makeTileFile(fullyQualifiedPath: String, sourceManagedDir: File): Seq[File] = {
    val file: File =
      sourceManagedDir / "Tile.scala"

    val newContents: String =
      templateTile(fullyQualifiedPath, CharMap.chars)

    IO.write(file, newContents)

    // println("Written: " + file.getCanonicalPath)

    Seq(file)
  }

  def templateRoguelikeTiles(
      fullyQualifiedPath: String,
      squareSizes: Seq[(Int, Int)],
      nonSquareSizes: Seq[(Int, Int)]
  ): String = {
    val contents =
      (squareSizes ++ nonSquareSizes)
        .map { case (charWidth, charHeight) =>
          val sizeString = s"${charWidth}x${charHeight}"
          s"""  val Size$sizeString: $fullyQualifiedPath.RoguelikeTiles$sizeString.type = $fullyQualifiedPath.RoguelikeTiles$sizeString"""
        }
        .mkString("\n")

    s"""package $fullyQualifiedPath
    |
    |// GENERATED by TileCharGen.scala - DO NOT EDIT
    |
    |object RoguelikeTiles {
    |
    |$contents
    |
    |}
    """.stripMargin
  }

  def makeRoguelikeTilesFile(
      fullyQualifiedPath: String,
      sourceManagedDir: File,
      squareSizes: Seq[(Int, Int)],
      nonSquareSizes: Seq[(Int, Int)]
  ): Seq[File] = {
    val file: File =
      sourceManagedDir / "RoguelikeTiles.scala"

    val newContents: String =
      templateRoguelikeTiles(fullyQualifiedPath, squareSizes, nonSquareSizes)

    IO.write(file, newContents)

    // println("Written: " + file.getCanonicalPath)

    Seq(file)
  }

  def gen(
      modulePrefix: String,
      fullyQualifiedPath: String,
      sourceManagedDir: File
  ): Seq[File] = {

    println("Generating Tiles & Fonts")

    // Sizes are from the Dwarf Fortress Tileset Repo
    // https://dwarffortresswiki.org/Tileset_repository

    val squareSizes =
      Seq(
        (1, 1),
        (5, 5),
        (6, 6),
        (7, 7),
        (8, 8),
        (9, 9),
        (10, 10),
        (11, 11),
        (12, 12),
        (13, 13),
        (14, 14),
        (15, 15),
        (16, 16),
        (18, 18),
        (20, 20),
        (24, 24),
        (32, 32),
        (48, 48),
        (64, 64)
      )

    val nonSquareSizes =
      Seq(
        (4, 6),
        (5, 6),
        (6, 8),
        (6, 9),
        (6, 10),
        (8, 12),
        (8, 14),
        (8, 15),
        (8, 16),
        (9, 12),
        (9, 14),
        (9, 16),
        (10, 12),
        (10, 16),
        (12, 20),
        (14, 16),
        (16, 20),
        (16, 24),
        (16, 32),
        (20, 32),
        (24, 32),
        (24, 36),
        (48, 72)
      )

    makeTileFile(fullyQualifiedPath, sourceManagedDir) ++
      makeRoguelikeTilesFile(fullyQualifiedPath, sourceManagedDir, squareSizes, nonSquareSizes) ++
      (squareSizes ++ nonSquareSizes).map { case (charWidth, charHeight) =>
        val sizeString = s"${charWidth}x${charHeight}"

        // println("Generating Tiles & Fonts - " + sizeString)

        val moduleName = modulePrefix + sizeString

        val contents: String =
          genFont(charWidth, charHeight, CharMap.chars) + "\n" + genTileCrops(
            charWidth,
            charHeight,
            CharMap.chars
          )

        val file: File =
          sourceManagedDir / (moduleName + ".scala")

        val newContents: String =
          template(moduleName, fullyQualifiedPath, contents)

        IO.write(file, newContents)

        // println("Written: " + file.getCanonicalPath)

        file
      }
  }

}

final case class CharDetail(index: Int, unicode: Int, char: Char, name: String) {

  def toCrop(charWidth: Int, charHeight: Int): (Int, Int, Int, Int) = {
    val x: Int = (index % 16) * charWidth
    val y: Int = (index / 16) * charWidth

    (x, y, charWidth, charHeight)
  }

  def toFontChar(charWidth: Int, charHeight: Int): String = {
    val x: Int     = (index % 16) * charWidth
    val y: Int     = (index / 16) * charWidth
    val charString = if (name == "REVERSE_SOLIDUS") "\\\\" else char.toString()

    s"""FontChar("$charString", ${x.toString}, ${y.toString}, ${charWidth.toString}, ${charHeight.toString})"""
  }
}

object CharMap {

  val chars = List(
    CharDetail(0, 0x00, ' ', "NULL"),
    CharDetail(1, 0x263a, '☺', "WHITE_SMILING_FACE"),
    CharDetail(2, 0x263b, '☻', "BLACK_SMILING_FACE"),
    CharDetail(3, 0x2665, '♥', "BLACK_HEART_SUIT"),
    CharDetail(4, 0x2666, '♦', "BLACK_DIAMOND_SUIT"),
    CharDetail(5, 0x2663, '♣', "BLACK_CLUB_SUIT"),
    CharDetail(6, 0x2660, '♠', "BLACK_SPADE_SUIT"),
    CharDetail(7, 0x2022, '•', "BULLET"),
    CharDetail(8, 0x25d8, '◘', "INVERSE_BULLET"),
    CharDetail(9, 0x25cb, '○', "WHITE_CIRCLE"),
    CharDetail(10, 0x25d9, '◙', "INVERSE_WHITE_CIRCLE"),
    CharDetail(11, 0x2642, '♂', "MALE_SIGN"),
    CharDetail(12, 0x2640, '♀', "FEMALE_SIGN"),
    CharDetail(13, 0x266a, '♪', "EIGHTH_NOTE"),
    CharDetail(14, 0x266b, '♫', "BEAMED_EIGHTH_NOTES"),
    CharDetail(15, 0x263c, '☼', "WHITE_SUN_WITH_RAYS"),
    CharDetail(16, 0x25ba, '►', "BLACK_RIGHT_POINTING_POINTER"),
    CharDetail(17, 0x25c4, '◄', "BLACK_LEFT_POINTING_POINTER"),
    CharDetail(18, 0x2195, '↕', "UP_DOWN_ARROW"),
    CharDetail(19, 0x203c, '‼', "DOUBLE_EXCLAMATION_MARK"),
    CharDetail(20, 0xb6, '¶', "PILCROW_SIGN"),
    CharDetail(21, 0xa7, '§', "SECTION_SIGN"),
    CharDetail(22, 0x25ac, '▬', "BLACK_RECTANGLE"),
    CharDetail(23, 0x21a8, '↨', "UP_DOWN_ARROW_WITH_BASE"),
    CharDetail(24, 0x2191, '↑', "UPWARDS_ARROW"),
    CharDetail(25, 0x2193, '↓', "DOWNWARDS_ARROW"),
    CharDetail(26, 0x2192, '→', "RIGHTWARDS_ARROW"),
    CharDetail(27, 0x2190, '←', "LEFTWARDS_ARROW"),
    CharDetail(28, 0x221f, '∟', "RIGHT_ANGLE"),
    CharDetail(29, 0x2194, '↔', "LEFT_RIGHT_ARROW"),
    CharDetail(30, 0x25b2, '▲', "BLACK_UP_POINTING_TRIANGLE"),
    CharDetail(31, 0x25bc, '▼', "BLACK_DOWN_POINTING_TRIANGLE"),
    CharDetail(32, 0x20, ' ', "SPACE"),
    CharDetail(33, 0x21, '!', "EXCLAMATION_MARK"),
    CharDetail(34, 0x22, '”', "QUOTATION_MARK"),
    CharDetail(35, 0x23, '#', "NUMBER_SIGN"),
    CharDetail(36, 0x24, '$', "DOLLAR_SIGN"),
    CharDetail(37, 0x25, '%', "PERCENT_SIGN"),
    CharDetail(38, 0x26, '&', "AMPERSAND"),
    CharDetail(39, 0x27, '’', "APOSTROPHE"),
    CharDetail(40, 0x28, '(', "LEFT_PARENTHESIS"),
    CharDetail(41, 0x29, ')', "RIGHT_PARENTHESIS"),
    CharDetail(42, 0x2a, '*', "ASTERISK"),
    CharDetail(43, 0x2b, '+', "PLUS_SIGN"),
    CharDetail(44, 0x2c, ',', "COMMA"),
    CharDetail(45, 0x2d, '-', "HYPHEN_MINUS"),
    CharDetail(46, 0x2e, '.', "FULL_STOP"),
    CharDetail(47, 0x2f, '/', "SOLIDUS"),
    CharDetail(48, 0x30, '0', "DIGIT_ZERO"),
    CharDetail(49, 0x31, '1', "DIGIT_ONE"),
    CharDetail(50, 0x32, '2', "DIGIT_TWO"),
    CharDetail(51, 0x33, '3', "DIGIT_THREE"),
    CharDetail(52, 0x34, '4', "DIGIT_FOUR"),
    CharDetail(53, 0x35, '5', "DIGIT_FIVE"),
    CharDetail(54, 0x36, '6', "DIGIT_SIX"),
    CharDetail(55, 0x37, '7', "DIGIT_SEVEN"),
    CharDetail(56, 0x38, '8', "DIGIT_EIGHT"),
    CharDetail(57, 0x39, '9', "DIGIT_NINE"),
    CharDetail(58, 0x3a, ':', "COLON"),
    CharDetail(59, 0x3b, ';', "SEMICOLON"),
    CharDetail(60, 0x3c, '<', "LESS_THAN_SIGN"),
    CharDetail(61, 0x3d, '=', "EQUALS_SIGN"),
    CharDetail(62, 0x3e, '>', "GREATER_THAN_SIGN"),
    CharDetail(63, 0x3f, '?', "QUESTION_MARK"),
    CharDetail(64, 0x40, '@', "COMMERCIAL_AT"),
    CharDetail(65, 0x41, 'A', "LATIN_CAPITAL_LETTER_A"),
    CharDetail(66, 0x42, 'B', "LATIN_CAPITAL_LETTER_B"),
    CharDetail(67, 0x43, 'C', "LATIN_CAPITAL_LETTER_C"),
    CharDetail(68, 0x44, 'D', "LATIN_CAPITAL_LETTER_D"),
    CharDetail(69, 0x45, 'E', "LATIN_CAPITAL_LETTER_E"),
    CharDetail(70, 0x46, 'F', "LATIN_CAPITAL_LETTER_F"),
    CharDetail(71, 0x47, 'G', "LATIN_CAPITAL_LETTER_G"),
    CharDetail(72, 0x48, 'H', "LATIN_CAPITAL_LETTER_H"),
    CharDetail(73, 0x49, 'I', "LATIN_CAPITAL_LETTER_I"),
    CharDetail(74, 0x4a, 'J', "LATIN_CAPITAL_LETTER_J"),
    CharDetail(75, 0x4b, 'K', "LATIN_CAPITAL_LETTER_K"),
    CharDetail(76, 0x4c, 'L', "LATIN_CAPITAL_LETTER_L"),
    CharDetail(77, 0x4d, 'M', "LATIN_CAPITAL_LETTER_M"),
    CharDetail(78, 0x4e, 'N', "LATIN_CAPITAL_LETTER_N"),
    CharDetail(79, 0x4f, 'O', "LATIN_CAPITAL_LETTER_O"),
    CharDetail(80, 0x50, 'P', "LATIN_CAPITAL_LETTER_P"),
    CharDetail(81, 0x51, 'Q', "LATIN_CAPITAL_LETTER_Q"),
    CharDetail(82, 0x52, 'R', "LATIN_CAPITAL_LETTER_R"),
    CharDetail(83, 0x53, 'S', "LATIN_CAPITAL_LETTER_S"),
    CharDetail(84, 0x54, 'T', "LATIN_CAPITAL_LETTER_T"),
    CharDetail(85, 0x55, 'U', "LATIN_CAPITAL_LETTER_U"),
    CharDetail(86, 0x56, 'V', "LATIN_CAPITAL_LETTER_V"),
    CharDetail(87, 0x57, 'W', "LATIN_CAPITAL_LETTER_W"),
    CharDetail(88, 0x58, 'X', "LATIN_CAPITAL_LETTER_X"),
    CharDetail(89, 0x59, 'Y', "LATIN_CAPITAL_LETTER_Y"),
    CharDetail(90, 0x5a, 'Z', "LATIN_CAPITAL_LETTER_Z"),
    CharDetail(91, 0x5b, '[', "LEFT_SQUARE_BRACKET"),
    CharDetail(92, 0x5c, '\\', "REVERSE_SOLIDUS"),
    CharDetail(93, 0x5d, ']', "RIGHT_SQUARE_BRACKET"),
    CharDetail(94, 0x5e, '^', "CIRCUMFLEX_ACCENT"),
    CharDetail(95, 0x5f, '_', "LOW_LINE"),
    CharDetail(96, 0x60, '`', "GRAVE_ACCENT"),
    CharDetail(97, 0x61, 'a', "LATIN_SMALL_LETTER_A"),
    CharDetail(98, 0x62, 'b', "LATIN_SMALL_LETTER_B"),
    CharDetail(99, 0x63, 'c', "LATIN_SMALL_LETTER_C"),
    CharDetail(100, 0x64, 'd', "LATIN_SMALL_LETTER_D"),
    CharDetail(101, 0x65, 'e', "LATIN_SMALL_LETTER_E"),
    CharDetail(102, 0x66, 'f', "LATIN_SMALL_LETTER_F"),
    CharDetail(103, 0x67, 'g', "LATIN_SMALL_LETTER_G"),
    CharDetail(104, 0x68, 'h', "LATIN_SMALL_LETTER_H"),
    CharDetail(105, 0x69, 'i', "LATIN_SMALL_LETTER_I"),
    CharDetail(106, 0x6a, 'j', "LATIN_SMALL_LETTER_J"),
    CharDetail(107, 0x6b, 'k', "LATIN_SMALL_LETTER_K"),
    CharDetail(108, 0x6c, 'l', "LATIN_SMALL_LETTER_L"),
    CharDetail(109, 0x6d, 'm', "LATIN_SMALL_LETTER_M"),
    CharDetail(110, 0x6e, 'n', "LATIN_SMALL_LETTER_N"),
    CharDetail(111, 0x6f, 'o', "LATIN_SMALL_LETTER_O"),
    CharDetail(112, 0x70, 'p', "LATIN_SMALL_LETTER_P"),
    CharDetail(113, 0x71, 'q', "LATIN_SMALL_LETTER_Q"),
    CharDetail(114, 0x72, 'r', "LATIN_SMALL_LETTER_R"),
    CharDetail(115, 0x73, 's', "LATIN_SMALL_LETTER_S"),
    CharDetail(116, 0x74, 't', "LATIN_SMALL_LETTER_T"),
    CharDetail(117, 0x75, 'u', "LATIN_SMALL_LETTER_U"),
    CharDetail(118, 0x76, 'v', "LATIN_SMALL_LETTER_V"),
    CharDetail(119, 0x77, 'w', "LATIN_SMALL_LETTER_W"),
    CharDetail(120, 0x78, 'x', "LATIN_SMALL_LETTER_X"),
    CharDetail(121, 0x79, 'y', "LATIN_SMALL_LETTER_Y"),
    CharDetail(122, 0x7a, 'z', "LATIN_SMALL_LETTER_Z"),
    CharDetail(123, 0x7b, '{', "LEFT_CURLY_BRACKET"),
    CharDetail(124, 0x7c, '|', "VERTICAL_LINE"),
    CharDetail(125, 0x7d, '}', "RIGHT_CURLY_BRACKET"),
    CharDetail(126, 0x7e, '~', "TILDE"),
    CharDetail(127, 0x7f, '⌂', "HOUSE"),
    CharDetail(128, 0xc7, 'Ç', "LATIN_CAPITAL_LETTER_C_WITH_CEDILLA"),
    CharDetail(129, 0xfc, 'ü', "LATIN_SMALL_LETTER_U_WITH_DIAERESIS"),
    CharDetail(130, 0xe9, 'é', "LATIN_SMALL_LETTER_E_WITH_ACUTE"),
    CharDetail(131, 0xe2, 'â', "LATIN_SMALL_LETTER_A_WITH_CIRCUMFLEX"),
    CharDetail(132, 0xe4, 'ä', "LATIN_SMALL_LETTER_A_WITH_DIAERESIS"),
    CharDetail(133, 0xe0, 'à', "LATIN_SMALL_LETTER_A_WITH_GRAVE"),
    CharDetail(134, 0xe5, 'å', "LATIN_SMALL_LETTER_A_WITH_RING_ABOVE"),
    CharDetail(135, 0xe7, 'ç', "LATIN_SMALL_LETTER_C_WITH_CEDILLA"),
    CharDetail(136, 0xea, 'ê', "LATIN_SMALL_LETTER_E_WITH_CIRCUMFLEX"),
    CharDetail(137, 0xeb, 'ë', "LATIN_SMALL_LETTER_E_WITH_DIAERESIS"),
    CharDetail(138, 0xe8, 'è', "LATIN_SMALL_LETTER_E_WITH_GRAVE"),
    CharDetail(139, 0xef, 'ï', "LATIN_SMALL_LETTER_I_WITH_DIAERESIS"),
    CharDetail(140, 0xee, 'î', "LATIN_SMALL_LETTER_I_WITH_CIRCUMFLEX"),
    CharDetail(141, 0xec, 'ì', "LATIN_SMALL_LETTER_I_WITH_GRAVE"),
    CharDetail(142, 0xc4, 'Ä', "LATIN_CAPITAL_LETTER_A_WITH_DIAERESIS"),
    CharDetail(143, 0xc5, 'Å', "LATIN_CAPITAL_LETTER_A_WITH_RING_ABOVE"),
    CharDetail(144, 0xc9, 'É', "LATIN_CAPITAL_LETTER_E_WITH_ACUTE"),
    CharDetail(145, 0xe6, 'æ', "LATIN_SMALL_LETTER_AE"),
    CharDetail(146, 0xc6, 'Æ', "LATIN_CAPITAL_LETTER_AE"),
    CharDetail(147, 0xf4, 'ô', "LATIN_SMALL_LETTER_O_WITH_CIRCUMFLEX"),
    CharDetail(148, 0xf6, 'ö', "LATIN_SMALL_LETTER_O_WITH_DIAERESIS"),
    CharDetail(149, 0xf2, 'ò', "LATIN_SMALL_LETTER_O_WITH_GRAVE"),
    CharDetail(150, 0xfb, 'û', "LATIN_SMALL_LETTER_U_WITH_CIRCUMFLEX"),
    CharDetail(151, 0xf9, 'ù', "LATIN_SMALL_LETTER_U_WITH_GRAVE"),
    CharDetail(152, 0xff, 'ÿ', "LATIN_SMALL_LETTER_Y_WITH_DIAERESIS"),
    CharDetail(153, 0xd6, 'Ö', "LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS"),
    CharDetail(154, 0xdc, 'Ü', "LATIN_CAPITAL_LETTER_U_WITH_DIAERESIS"),
    CharDetail(155, 0xa2, '¢', "CENT_SIGN"),
    CharDetail(156, 0xa3, '£', "POUND_SIGN"),
    CharDetail(157, 0xa5, '¥', "YEN_SIGN"),
    CharDetail(158, 0x20a7, '₧', "PESETA_SIGN"),
    CharDetail(159, 0x0192, 'ƒ', "LATIN_SMALL_LETTER_F_WITH_HOOK"),
    CharDetail(160, 0xe1, 'á', "LATIN_SMALL_LETTER_A_WITH_ACUTE"),
    CharDetail(161, 0xed, 'í', "LATIN_SMALL_LETTER_I_WITH_ACUTE"),
    CharDetail(162, 0xf3, 'ó', "LATIN_SMALL_LETTER_O_WITH_ACUTE"),
    CharDetail(163, 0xfa, 'ú', "LATIN_SMALL_LETTER_U_WITH_ACUTE"),
    CharDetail(164, 0xf1, 'ñ', "LATIN_SMALL_LETTER_N_WITH_TILDE"),
    CharDetail(165, 0xd1, 'Ñ', "LATIN_CAPITAL_LETTER_N_WITH_TILDE"),
    CharDetail(166, 0xaa, 'ª', "FEMININE_ORDINAL_INDICATOR"),
    CharDetail(167, 0xba, 'º', "MASCULINE_ORDINAL_INDICATOR"),
    CharDetail(168, 0xbf, '¿', "INVERTED_QUESTION_MARK"),
    CharDetail(169, 0x2310, '⌐', "REVERSED_NOT_SIGN"),
    CharDetail(170, 0xac, '¬', "NOT_SIGN"),
    CharDetail(171, 0xbd, '½', "VULGAR_FRACTION_ONE_HALF"),
    CharDetail(172, 0xbc, '¼', "VULGAR_FRACTION_ONE_QUARTER"),
    CharDetail(173, 0xa1, '¡', "INVERTED_EXCLAMATION_MARK"),
    CharDetail(174, 0xab, '«', "LEFT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK"),
    CharDetail(175, 0xbb, '»', "RIGHT_POINTING_DOUBLE_ANGLE_QUOTATION_MARK"),
    CharDetail(176, 0x2591, '░', "LIGHT_SHADE"),
    CharDetail(177, 0x2592, '▒', "MEDIUM_SHADE"),
    CharDetail(178, 0x2593, '▓', "DARK_SHADE"),
    CharDetail(179, 0x2502, '│', "BOX_DRAWINGS_LIGHT_VERTICAL"),
    CharDetail(180, 0x2524, '┤', "BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT"),
    CharDetail(181, 0x2561, '╡', "BOX_DRAWINGS_VERTICAL_SINGLE_AND_LEFT_DOUBLE"),
    CharDetail(182, 0x2562, '╢', "BOX_DRAWINGS_VERTICAL_DOUBLE_AND_LEFT_SINGLE"),
    CharDetail(183, 0x2556, '╖', "BOX_DRAWINGS_DOWN_DOUBLE_AND_LEFT_SINGLE"),
    CharDetail(184, 0x2555, '╕', "BOX_DRAWINGS_DOWN_SINGLE_AND_LEFT_DOUBLE"),
    CharDetail(185, 0x2563, '╣', "BOX_DRAWINGS_DOUBLE_VERTICAL_AND_LEFT"),
    CharDetail(186, 0x2551, '║', "BOX_DRAWINGS_DOUBLE_VERTICAL"),
    CharDetail(187, 0x2557, '╗', "BOX_DRAWINGS_DOUBLE_DOWN_AND_LEFT"),
    CharDetail(188, 0x255d, '╝', "BOX_DRAWINGS_DOUBLE_UP_AND_LEFT"),
    CharDetail(189, 0x255c, '╜', "BOX_DRAWINGS_UP_DOUBLE_AND_LEFT_SINGLE"),
    CharDetail(190, 0x255b, '╛', "BOX_DRAWINGS_UP_SINGLE_AND_LEFT_DOUBLE"),
    CharDetail(191, 0x2510, '┐', "BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT"),
    CharDetail(192, 0x2514, '└', "BOX_DRAWINGS_LIGHT_UP_AND_RIGHT"),
    CharDetail(193, 0x2534, '┴', "BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL"),
    CharDetail(194, 0x252c, '┬', "BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL"),
    CharDetail(195, 0x251c, '├', "BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT"),
    CharDetail(196, 0x2500, '─', "BOX_DRAWINGS_LIGHT_HORIZONTAL"),
    CharDetail(197, 0x253c, '┼', "BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL"),
    CharDetail(198, 0x255e, '╞', "BOX_DRAWINGS_VERTICAL_SINGLE_AND_RIGHT_DOUBLE"),
    CharDetail(199, 0x255f, '╟', "BOX_DRAWINGS_VERTICAL_DOUBLE_AND_RIGHT_SINGLE"),
    CharDetail(200, 0x255a, '╚', "BOX_DRAWINGS_DOUBLE_UP_AND_RIGHT"),
    CharDetail(201, 0x2554, '╔', "BOX_DRAWINGS_DOUBLE_DOWN_AND_RIGHT"),
    CharDetail(202, 0x2569, '╩', "BOX_DRAWINGS_DOUBLE_UP_AND_HORIZONTAL"),
    CharDetail(203, 0x2566, '╦', "BOX_DRAWINGS_DOUBLE_DOWN_AND_HORIZONTAL"),
    CharDetail(204, 0x2560, '╠', "BOX_DRAWINGS_DOUBLE_VERTICAL_AND_RIGHT"),
    CharDetail(205, 0x2550, '═', "BOX_DRAWINGS_DOUBLE_HORIZONTAL"),
    CharDetail(206, 0x256c, '╬', "BOX_DRAWINGS_DOUBLE_VERTICAL_AND_HORIZONTAL"),
    CharDetail(207, 0x2567, '╧', "BOX_DRAWINGS_UP_SINGLE_AND_HORIZONTAL_DOUBLE"),
    CharDetail(208, 0x2568, '╨', "BOX_DRAWINGS_UP_DOUBLE_AND_HORIZONTAL_SINGLE"),
    CharDetail(209, 0x2564, '╤', "BOX_DRAWINGS_DOWN_SINGLE_AND_HORIZONTAL_DOUBLE"),
    CharDetail(210, 0x2565, '╥', "BOX_DRAWINGS_DOWN_DOUBLE_AND_HORIZONTAL_SINGLE"),
    CharDetail(211, 0x2559, '╙', "BOX_DRAWINGS_UP_DOUBLE_AND_RIGHT_SINGLE"),
    CharDetail(212, 0x2558, '╘', "BOX_DRAWINGS_UP_SINGLE_AND_RIGHT_DOUBLE"),
    CharDetail(213, 0x2552, '╒', "BOX_DRAWINGS_DOWN_SINGLE_AND_RIGHT_DOUBLE"),
    CharDetail(214, 0x2553, '╓', "BOX_DRAWINGS_DOWN_DOUBLE_AND_RIGHT_SINGLE"),
    CharDetail(215, 0x256b, '╫', "BOX_DRAWINGS_VERTICAL_DOUBLE_AND_HORIZONTAL_SINGLE"),
    CharDetail(216, 0x256a, '╪', "BOX_DRAWINGS_VERTICAL_SINGLE_AND_HORIZONTAL_DOUBLE"),
    CharDetail(217, 0x2518, '┘', "BOX_DRAWINGS_LIGHT_UP_AND_LEFT"),
    CharDetail(218, 0x250c, '┌', "BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT"),
    CharDetail(219, 0x2588, '█', "FULL_BLOCK"),
    CharDetail(220, 0x2584, '▄', "LOWER_HALF_BLOCK"),
    CharDetail(221, 0x258c, '▌', "LEFT_HALF_BLOCK"),
    CharDetail(222, 0x2590, '▐', "RIGHT_HALF_BLOCK"),
    CharDetail(223, 0x2580, '▀', "UPPER_HALF_BLOCK"),
    CharDetail(224, 0x03b1, 'α', "GREEK_SMALL_LETTER_ALPHA"),
    CharDetail(225, 0xdf, 'ß', "LATIN_SMALL_LETTER_SHARP_S"),
    CharDetail(226, 0x0393, 'Γ', "GREEK_CAPITAL_LETTER_GAMMA"),
    CharDetail(227, 0x03c0, 'π', "GREEK_SMALL_LETTER_PI"),
    CharDetail(228, 0x03a3, 'Σ', "GREEK_CAPITAL_LETTER_SIGMA"),
    CharDetail(229, 0x03c3, 'σ', "GREEK_SMALL_LETTER_SIGMA"),
    CharDetail(230, 0xb5, 'µ', "MICRO_SIGN"),
    CharDetail(231, 0x03c4, 'τ', "GREEK_SMALL_LETTER_TAU"),
    CharDetail(232, 0x03a6, 'Φ', "GREEK_CAPITAL_LETTER_PHI"),
    CharDetail(233, 0x0398, 'Θ', "GREEK_CAPITAL_LETTER_THETA"),
    CharDetail(234, 0x03a9, 'Ω', "GREEK_CAPITAL_LETTER_OMEGA"),
    CharDetail(235, 0x03b4, 'δ', "GREEK_SMALL_LETTER_DELTA"),
    CharDetail(236, 0x221e, '∞', "INFINITY"),
    CharDetail(237, 0x03c6, 'φ', "GREEK_SMALL_LETTER_PHI"),
    CharDetail(238, 0x03b5, 'ε', "GREEK_SMALL_LETTER_EPSILON"),
    CharDetail(239, 0x2229, '∩', "INTERSECTION"),
    CharDetail(240, 0x2261, '≡', "IDENTICAL_TO"),
    CharDetail(241, 0xb1, '±', "PLUS_MINUS_SIGN"),
    CharDetail(242, 0x2265, '≥', "GREATER_THAN_OR_EQUAL_TO"),
    CharDetail(243, 0x2264, '≤', "LESS_THAN_OR_EQUAL_TO"),
    CharDetail(244, 0x2320, '⌠', "TOP_HALF_INTEGRAL"),
    CharDetail(245, 0x2321, '⌡', "BOTTOM_HALF_INTEGRAL"),
    CharDetail(246, 0xf7, '÷', "DIVISION_SIGN"),
    CharDetail(247, 0x2248, '≈', "ALMOST_EQUAL_TO"),
    CharDetail(248, 0xb0, '°', "DEGREE_SIGN"),
    CharDetail(249, 0x2219, '∙', "BULLET_OPERATOR"),
    CharDetail(250, 0xb7, '·', "MIDDLE_DOT"),
    CharDetail(251, 0x221a, '√', "SQUARE_ROOT"),
    CharDetail(252, 0x207f, 'ⁿ', "SUPERSCRIPT_LATIN_SMALL_LETTER_N"),
    CharDetail(253, 0xb2, '²', "SUPERSCRIPT_TWO"),
    CharDetail(254, 0x25a0, '■', "BLACK_SQUARE"),
    CharDetail(255, 0xa0, ' ', "NO_BREAK_SPACE")
  )

}
