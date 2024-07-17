package com.alvinalexander.utils

import java.io.StringWriter
import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


/**
 * TODO: Some (many) of these functions need unit tests.
 */
object StringUtils {

    /**
     * Returns a string that is the same as the input string, but
     * truncated to the specified length.
     * {{{
     * val y = StringUtils.truncate("Alvin", 2)
     * // result is "Al"
     * }}}
     * @param s A non-null input string.
     * @param length The maximum number of characters to return.
     */
    def truncate(s: String, length: Int): String = s.take(length)

    /**
     * Returns a string that is the same as the input string, but
     * truncated to the specified length, with three dots shown
     * after the specified string length.
     *
     * {{{
     * val y = StringUtils.truncateWithEllipsis("Alvin", 2)
     * // result is "Al..."
     * }}}
     *
     * @param s A non-null input string.
     * @param lengthRequested The maximum number of characters to return, expected to be `0` or
      *              higher (i.e., a non-negative number).
     */
    def truncateWithEllipsis(s: String, lengthRequested: Int): String = {
        if (s.length == 0) return ""
        if (lengthRequested == 0) return ""
        if (lengthRequested < s.length) {
            s.take(lengthRequested) + "..."
        } else if (lengthRequested == s.length) {
            s
        } else {
            s
        }
    }

    /**
      * Returns true if the string contains only letters and numbers.
      * @param s
      * @return
      */
    def lettersAndNumbersOnly_?(s: String): Boolean = s.matches("[a-zA-Z0-9]+")

    /**
      * Returns true if the string contains only letters (`[a-zA-Z]`).
      * Blank strings return false.
      * @param s
      * @return
      */
    def lettersOnly_?(s: String): Boolean = s.matches("[a-zA-Z]+")

    /**
      * Returns true if the string contains only numbers (`[0-9]`).
      * @param s
      * @return
      */
    def numbersOnly_?(s: String): Boolean = s.matches("[0-9]+")

    /**
     * Returns true if the given string contains any whitespace at all.
     * Assumes `s` is not null.
     */
    def containsWhitespace(s: String): Boolean = s.matches(".*\\s.*")

    /**
     * Returns true if the given string would be interpreted as being
     * only blanks or whitespace.
     * 
     * Assumes `s` is not null.
     */
    def isBlank(s: String): Boolean = s.trim == ""

    /**
     * A 'sanitize' method. Takes an input string, and returns a
     * new string with all characters removed from that string
     * other than letters and numbers. Note that it also removes blank spaces.
     * {{{
     * val y = StringUtils.removeAllButLettersAndNumbers("`;|hack attempt!;`")
     * // result is "hackattempt"
     * }}}
     */
    def removeAllButLettersAndNumbers(s: String): String = replaceAll(s, "[^a-zA-Z0-9]", "")

    /**
     * @param s The string to perform the replace operation on (such as "123 Main Street")
     * @param regex The regular expression to use to find what you want to replace.
     * @param replaceWith The string you want to use as the replacement. Can be an empty string,
     * asterisk, etc., anything you want to use as the replacement pattern.
     */
    def replaceAll(s: String, regex: String, replaceWith: String): String = {
        val r = regex.r
        r.replaceAllIn(s, replaceWith)
    }

    /**
      * Returns true if the string is null or empty. An “empty” string may contain
      * blank spaces and characters like `\n`, `\r`, and `\t`.
      * @param s
      * @return
      */
    def isNullOrEmpty(s: String): Boolean = if (s==null || s.trim.equals("")) true else false

    /**
      * Trim all spaces at the beginning of a string (the left side of the string).
      * Example: `  foo` becomes `foo`.
      */
    def leftTrim(s: String): String = s.replaceAll("^\\s+", "")

    /**
      * Trim all spaces at the end of a string (the right side of the string).
      * Example: `foo  ` becomes `foo`.
      */
    def rightTrim(s: String): String = s.replaceAll("\\s+$", "")

    /**
      * This method capitalizes all words in a string where the words are separated by blank
      * spaces. It does not currently work with characters like \n, \r, and \t.
      *
      * Example: `foo bar baz` returns `Foo Bar Baz`.
      */
    def capitalizeAllWordsInString(s: String): String = s.split(" ").map(_.capitalize).mkString(" ")

    //TODO if you want to pursue this, need to capture the whitespace characters in the input string
    //     and then replace them in the output string
    // def capitalizeAllWordsInString(s: String): String = {
    //     s.split("(\\p{Space})").map(_.capitalize).mkString("$1")
    //}


    /**
      * This is the best way I know to convert a stack trace into a string.
      * @param t Any type of throwable.
      * @return A string representation of the throwable’s stack trace.
      */
    def getStackTraceAsString(t: Throwable): String = {
        val sw = new StringWriter
        t.printStackTrace(new PrintWriter(sw))
        sw.toString
    }

    /**
      * Splits a CamelCase string into words such as splitting
      * "FooBarBaz" into "Foo Bar Baz".
      *
      * @param s A non-null, CamelCase string.
      * @return A camel-case string separated into words, like "Foo Bar Baz".
      */
    def splitCamelCase(s: String): String = {
        return s.replaceAll(
            String.format(
                "%s|%s|%s",
                "(?<=[A-Z])(?=[A-Z][a-z])",
                "(?<=[^A-Z])(?=[A-Z])",
                "(?<=[A-Za-z])(?=[^A-Za-z])"
            ),
            " "
            ).replaceAll("  ", " ")
    }

    /**
      * Generates a random-length string of _printable_ characters
      * (Random.nextPrintableChar), with about 20% of them being blank spaces.
      *
      * TODO let the caller specify the values that are currently hard-coded.
      *
      * @param r
      * @return
      */
    def genRandomVariableLengthStringWithBlankSpaces(r: scala.util.Random): String = {
        val ab = ArrayBuffer[Char]()
        val maxLength = r.nextInt(100) + 50  //range of 50-149
        for (i <- 1 to maxLength) {
            if (i % 5 == 0)
                ab.append(' ')
            else
                ab.append(r.nextPrintableChar)
        }
        val charSeq: mutable.Seq[Char] = Random.shuffle(ab)
        charSeq.mkString + "\n"
    }

    /**
      * Generates a random alphanumeric string using `Random.alphanumeric` of the length
      * specified.
      *
      * @param length The desired length of the string.
      */
    def randomAlphanumericString(length: Int): String = Random.alphanumeric.take(length).mkString

//    /**
//      * Generates a random alphanumeric string of the length specified.
//      * @param length The desired length of the string.
//      */
//    //TODO test
//    def randomAlphaNumericString(length: Int): String = {
//        val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
//        randomStringFromCharList(length, chars)
//    }

    /**
      * Generates a random alpha ('a' to 'Z') string of the length specified.
      * @param length The desired length of the string.
      */
    //TODO test
    def randomAlphaString(length: Int): String = {
        val chars = ('a' to 'z') ++ ('A' to 'Z')
        randomStringFromCharList(length, chars)
    }

    private def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
        val sb = new StringBuilder
        for (i <- 1 to length) {
            val randomNum = util.Random.nextInt(chars.length)
            sb.append(chars(randomNum))
        }
        sb.toString
    }

    /**
      * "foo\nbar\nbaz" becomes `Seq("foo", "bar", "baz")`.
      * Currently works well with strings like that, might give unexpected
      * results with something like `"foo\nbar\n \n " `
      *
      */
    def convertStringWithNewlinesToSeq(s: String): Seq[String] = s.split("\n").toVector

    /**
      * Converts `fooBarBaz` to `foo_bar_baz`.
      */
    def convertCamelCaseToUnderscore(s: String): String = s.replaceAll("([A-Z])", "_" + "$1").toLowerCase

    /**
      * Converts `foo_bar_baz` to `fooBarBaz`.
      * This code was converted from Java by IntelliJ; need to correct it.
      */
//    def convertUnderscoreNameToUpperCase(s: String): String = {
//        val sb = new StringBuffer
//        var i = 0
//        while ( {
//            i < s.length
//        }) {
//            var c = s.charAt(i)
//            if (c == '_') { // don't add underscore to the new name, but make the next
//                // character uppercase
//                i += 1
//                c = s.charAt(i)
//                sb.append(Character.toUpperCase(c))
//            }
//            else sb.append(c) {
//                i += 1; i - 1
//            }
//        }
//        sb.toString
//    }

    /**
      * Removes the trailing 's' from a string. Technically all it does right now is
      * drop the last character from the string. What I probably need are `pluralize`
      * and `dePluralize` functions.
      */
    def removeTrailingS(s: String): String = s.dropRight(1)

    /**
     * Converts a boolean to a "y" (true) or "n" (false).
     */
    def booleanAsYOrN(b: Boolean): String = if (b) "y" else "n"

    /**
     * Returns `true` if the input string matches "y" or "yes".
     * I use this when prompting users for Y/N input at the command line.
     * `default` is used when the user types something other than 
     * `[y|yes|n|no]`, such as when they hit [Enter] at the command line.
     * 
     * Assumes `userInput` is not null.
     */
    def isYes(userInput: String, default: Boolean): Boolean =
        if (userInput.trim.equalsIgnoreCase("Y") || userInput.trim.equalsIgnoreCase("yes")) {
            true
        }
        else if (userInput.trim.equalsIgnoreCase("N") || userInput.trim.equalsIgnoreCase("no")) {
            false
        }
        else if (isBlank(userInput)) {
            default
        } else {
            // note that the user can type in "foo" or anything else
            default
        }

    /**
     * Expects a one-line string as input, and returns a two-line string
     * as output.
     * @param s Like "Hi there."
     * @return A string like "Hi there.\n---------".
     */
    def addAsciiUnderlineToString(s: String): String = {
        val underline = "-" * s.length
        s"$s\n$underline"
    }

    /**
     * Formats the given string so it is right-padded to a string
     * that has the width specified. Ex: Given ("AAPL", 7) as
     * input, it returns "   AAPL".
     * @param s The input string.
     * @param width The desired width. Should be > s.length.
     * @return A left-padded, right-justified string.
     */
    def rightJustify(input: String, width: Int): String =
        String.format(s"%${width}s", input)

    /**
     * Formats the given string so it is left-padded to a string
     * that has the width specified. Ex: Given ("AAPL", 7) as
     * input, it returns "AAPL   ".
     *
     * @param s The input string.
     * @param width The desired width. Should be > s.length.
     * @return A right-padded, left-justified string.
     *
     * Note that the 'f' interpolator does not work in this case:
     *     f"$input%${width}s"
     */
    def leftJustify(input: String, width: Int): String =
        String.format(s"%-${width}s", input)

    def removeBlankStrings(strings: Seq[String]) =
        strings.filterNot(_.trim.equals(""))

    def stringToBoolean(s: String): Boolean =
        if s.trim.toUpperCase == "TRUE" then true else false

    /**
     * Merges "123\n456\n789" and "aaa\nbbb\nccc" into
     * "123aaa\n456bbb\n789ccc".
     */
    def mergeMultilineStrings(s1: String, s2: String): String = {
        val xsLines: Seq[String] = xs.split("\n").toSeq
        val ysLines: Seq[String] = ys.split("\n").toSeq
        val zippedLines: Seq[(String, String)] = xsLines.zip(ysLines)
        val combinedLines: Seq[String] = zippedLines.map { case (x, y) => x + y }
        combinedLines.mkString("\n")
    }
    
}

