package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class ReplacerInputStreamTest extends AnyFunSuite {
  def replace(string: String, map: Map[String, String]): String = {
    val inputStreamOriginal = StringBytesUtils.stringToInputStream(string)
    val inputStreamReplaced = ReplacerInputStream.fromStringMap(inputStreamOriginal, map)
    StringBytesUtils.inputStreamToString(inputStreamReplaced)
  }
  def prettyPrintMap(map: Map[String, String]): String = {
    map.map {
      case (key, value) =>
        val q = '\"'
        s"$q$key$q -> $q$value$q"
    }.mkString("Map(", ", ", ")")
  }
  def assertReplaced(original: String, expected: String, map: Map[String, String]): Unit = {
    assertResult(expected, prettyPrintMap(map)) (replace(original, map))
  }

  test("Various replacements") {
    assertReplaced("Hello, World!", "Hello, World!", Map.empty)
    assertReplaced("Hello, World!", "Hellu, Wurld!", Map("o" -> "u"))
    assertReplaced("Hello, World!", "Hello, World!", Map("foo" -> "bar"))
    assertReplaced("Hello, World!", "Heo, Word!", Map("l" -> ""))
    assertReplaced("Hello, World!", "Hello, World!", Map("!" -> "!"))
    assertReplaced("Hello, World", "Hello, World", Map("!" -> ""))
    assertReplaced("Hello, World!", "Hello, World!!!", Map("!" -> "!!!"))
    assertReplaced("Hello, World!", "Holle, Werld!", Map("o" -> "e", "e" -> "o"))
    assertReplaced("Hello, World!", "Yo, World!", Map("Hello" -> "Yo"))
    assertReplaced("Hello, World!", "Hellllllo, Worllld!", Map("l" -> "lll"))
  }

}

