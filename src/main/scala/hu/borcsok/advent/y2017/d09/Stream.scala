package hu.borcsok.advent.y2017.d09

class Stream(val str: String, val skipped: Int) {

  def score: Int = str.foldLeft((0, 0)) {
    case ((result, depth), char) =>
      if (char == '{') (result + depth + 1, depth + 1)
      else if (char == '}') (result, depth - 1)
      else (result, depth)
  }._1

}

object Stream {

  def apply(input: String): Stream = {
    val (withoutSkippedChars, _) = input.foldLeft(("", false)) {
      case ((string, ignoreCharacter), char) =>
        if (ignoreCharacter) (string, false)
        else if (char == '!') (string, true)
        else (string + char, false)
    }

    val (withoutGarbage, _) = withoutSkippedChars.foldLeft(("", false)) {
      case ((string, insideGarbage), char) =>
        if (char == '>') (string + char, false)
        else if (char == '<' && !insideGarbage) (string + char, true)
        else if (insideGarbage) (string, true)
        else (string + char, false)
    }

    val withoutGarbageAndTags = withoutGarbage.foldLeft("") {
      case (string, char) =>
        if (char == '>' || char == '<') string
        else string + char
    }

    // println(s"$input -> $withoutSkippedChars -> $withoutGarbage -> $withoutGarbageAndTags")
    new Stream(withoutGarbageAndTags, withoutSkippedChars.length - withoutGarbage.length)
  }
}
