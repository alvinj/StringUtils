package com.alvinalexander.utils

import org.scalatest.{FunSuite, Pending}
import StringUtils._

class StringUtilsTests extends FunSuite {

    val nums = "0123456789"
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


    /**
      * truncate
      * --------
      */
    test("`truncate` throws NPE with a null") {
        val e: NullPointerException = intercept[NullPointerException] {
            val s = truncate(null, 1)
        }
        assert(e.isInstanceOf[NullPointerException])
    }

    test("`truncate` should handle an empty string") {
        var result = ""
        result = truncate("", 0);    assert(result == "")
        result = truncate("", 9);    assert(result == "")
    }

    test("`truncate`: strLen < desiredLen") {
        var result = ""
        result = truncate("a", 2);    assert(result == "a")
        result = truncate("ab", 3);   assert(result == "ab")
        result = truncate("abc", 9);  assert(result == "abc")
    }

    test("`truncate`: strLen > desiredLen") {
        var result = ""
        result = truncate("abc",  2);  assert(result == "ab")
        result = truncate("abcd", 3);  assert(result == "abc")
    }

    test("`truncate`: strLen == desiredLen") {
        var result = ""
        result = truncate("abc",  3);  assert(result == "abc")
        result = truncate("abcd", 4);  assert(result == "abcd")
    }

    test("`truncate` w/ desiredLen=0 should return an empty string") {
        var result = ""
        result = truncate("", 0);    assert(result == "")
        result = truncate("a", 0);   assert(result == "")
        result = truncate("aaa", 0); assert(result == "")
    }


    /**
      * truncateWithEllipsis
      * --------------------
      */
    test("`truncateWithEllipsis` throws NPE with a null") {
        val e: NullPointerException = intercept[NullPointerException] {
            val s = truncateWithEllipsis(null, 1)
        }
        assert(e.isInstanceOf[NullPointerException])
    }

    test("`truncateWithEllipsis`: normal expected behavior") {
        var result = ""
        result = truncateWithEllipsis("aleka", 1);   assert(result == "a...")
        result = truncateWithEllipsis("aleka", 2);   assert(result == "al...")
        result = truncateWithEllipsis("aleka", 3);   assert(result == "ale...")
        result = truncateWithEllipsis("aleka", 4);   assert(result == "alek...")
        result = truncateWithEllipsis("aleka", 5);   assert(result == "aleka")
        result = truncateWithEllipsis("aleka", 6);   assert(result == "aleka")

        result = truncateWithEllipsis("hi mom", 2);  assert(result == "hi...")
        result = truncateWithEllipsis("hi mom", 3);  assert(result == "hi ...")
        result = truncateWithEllipsis("hi mom", 4);  assert(result == "hi m...")
        result = truncateWithEllipsis("hi mom", 5);  assert(result == "hi mo...")
        result = truncateWithEllipsis("hi mom", 6);  assert(result == "hi mom")
        result = truncateWithEllipsis("hi mom", 7);  assert(result == "hi mom")
    }

    test("`truncateWithEllipsis` should handle an empty string") {
        val result = truncateWithEllipsis("", 2)
        assert(result == "")
    }

    test("`truncateWithEllipsis` w/ desiredLen=0 should return an empty string") {
        var result = ""
        result = truncateWithEllipsis("", 0);    assert(result == "")
        result = truncateWithEllipsis("a", 0);   assert(result == "")
        result = truncateWithEllipsis("aaa", 0); assert(result == "")
    }

    test("`truncateWithEllipsis`: if strLen<lenParam, return the string") {
        var result = ""
        result = truncateWithEllipsis("a", 9);   assert(result == "a")
        result = truncateWithEllipsis("ab", 3);  assert(result == "ab")
        result = truncateWithEllipsis("abc", 4); assert(result == "abc")
    }



    /**
      * lettersAndNumbersOnly_?
      * -----------------------
      */
    test("`lettersAndNumbersOnly_?` handles letters and numbers only") {
        var result = false
        result = lettersAndNumbersOnly_?(nums);        assert(result == true)
        result = lettersAndNumbersOnly_?(chars);       assert(result == true)
        result = lettersAndNumbersOnly_?(nums+chars);  assert(result == true)
        result = lettersAndNumbersOnly_?(chars+nums);  assert(result == true)
    }

    test("`lettersAndNumbersOnly_?` handles blank strings") {
        var result = false
        result = lettersAndNumbersOnly_?("");        assert(result == false)
        result = lettersAndNumbersOnly_?(" ");       assert(result == false)
        result = lettersAndNumbersOnly_?("\n\r\t");  assert(result == false)
    }

    test("`lettersAndNumbersOnly_?` handles non-alphanumeric") {
        var result = false
        result = lettersAndNumbersOnly_?(" ");     assert(result == false)
        result = lettersAndNumbersOnly_?("!");     assert(result == false)
        result = lettersAndNumbersOnly_?("@");     assert(result == false)
        result = lettersAndNumbersOnly_?("123$");  assert(result == false)
    }

    test("`lettersAndNumbersOnly_?` handles *all* non-alphanumeric") {
        val nonAlphaChars: List[Char] = (' ' to '/').toList ++:
            (':' to '@').toList ++:
            ('[' to '`').toList ++:
            ('{' to '~').toList
        var result = false
        for (c <- nonAlphaChars) {
            result = lettersAndNumbersOnly_?(c.toString);    assert(result == false)
            result = lettersAndNumbersOnly_?("a" + c + "b"); assert(result == false)
        }
    }



    /**
      * lettersOnly_?
      * -------------
      */
    //TODO update these tests to the same style as `lettersAndNumbersOnly_?`
    test("`lettersOnly_?` handles letters only") {
        var result = false
        result = lettersOnly_?(chars);          assert(result == true)
        result = lettersOnly_?(chars.reverse);  assert(result == true)
    }

    test("`lettersOnly_?` rejects numbers") {
        var result = false
        result = lettersOnly_?("1");        assert(result == false)
        result = lettersOnly_?("1a");       assert(result == false)
        result = lettersOnly_?("abc1");     assert(result == false)
        result = lettersOnly_?("abc1def");  assert(result == false)
    }

    test("`lettersOnly_?` rejects blank strings") {
        var result = false
        result = lettersOnly_?("");        assert(result == false)
        result = lettersOnly_?(" ");       assert(result == false)
        result = lettersOnly_?("\n\r\t");  assert(result == false)
    }


    /**
      * numbersOnly_?
      * -------------
      */
    //TODO update these tests to the same style as `lettersAndNumbersOnly_?`
    test("`numbersOnly_?` handles numbers only") {
        var result = false
        result = numbersOnly_?(nums);          assert(result == true)
        result = numbersOnly_?(nums.reverse);  assert(result == true)
    }

    test("`numbersOnly_?` rejects strings") {
        var result = false
        result = numbersOnly_?(chars);          assert(result == false)
        result = numbersOnly_?(chars.reverse);  assert(result == false)
    }

    test("`numbersOnly_?` rejects blank strings") {
        var result = false
        result = numbersOnly_?("");        assert(result == false)
        result = numbersOnly_?(" ");       assert(result == false)
        result = numbersOnly_?("\n\r\t");  assert(result == false)
    }



    /**
      * removeAllButLettersAndNumbers
      */
    test("`removeAllButLettersAndNumbers`: nums and chars only should return itself") {
        var result = ""
        result = removeAllButLettersAndNumbers(nums);        assert(result == nums)
        result = removeAllButLettersAndNumbers(chars);       assert(result == chars)
        result = removeAllButLettersAndNumbers(nums+chars);  assert(result == nums+chars)
        result = removeAllButLettersAndNumbers(chars+nums);  assert(result == chars+nums)
    }

    test("`removeAllButLettersAndNumbers`: rm all bad characters") {
        var result = ""
        result = removeAllButLettersAndNumbers(""";'[]\}{=+""")
        assert(result == "")

        result = removeAllButLettersAndNumbers("""`~!@#$%^&*()_+-=[]\{}|;'":,./<>?""")
        assert(result == "")

        result = removeAllButLettersAndNumbers("<script language=\"java\"")
        assert(result == "scriptlanguagejava")
    }

    test("`removeAllButLettersAndNumbers`: rm blank characters") {
        var result = ""
        result = removeAllButLettersAndNumbers("how to serve humans")
        assert(result == "howtoservehumans")

        result = removeAllButLettersAndNumbers("  how to serve humans  ")
        assert(result == "howtoservehumans")
    }

    test("`removeAllButLettersAndNumbers`: blank strings return 0-length string") {
        var result = ""
        result = removeAllButLettersAndNumbers(" ");       assert(result == "")
        result = removeAllButLettersAndNumbers("  ");      assert(result == "")
        result = removeAllButLettersAndNumbers(" \r\n\t"); assert(result == "")
    }


    /**
      * replaceAll
      * ----------
      */
    test("replaceAll: replace all characters other than letters and numbers") {
        assert(replaceAll("foo bar baz", "[^a-zA-Z0-9]", "*") == "foo*bar*baz")
        assert(replaceAll("<script language=\"java\"",  "[^a-zA-Z0-9]", "*") == "*script*language**java*")
        assert(replaceAll("  ", "[^a-zA-Z0-9]", "*") == "**")
        assert(replaceAll("<script ", "[<>]", "*") == "*script ")
        assert(replaceAll("/>", "[<>]", "*") == "/*")
        assert(replaceAll("<html>", "[<>]", "*") == "*html*")
    }


    /**
      * isNullOrEmpty
      * -------------
      */
    test("`isNullOrEmpty`: handles null and blank strings") {
        var result = false
        result = isNullOrEmpty(null);   assert(result == true)
        result = isNullOrEmpty("");     assert(result == true)
        result = isNullOrEmpty(" ");    assert(result == true)
        result = isNullOrEmpty("  ");   assert(result == true)
        result = isNullOrEmpty("\n");   assert(result == true)
        result = isNullOrEmpty("\r");   assert(result == true)
        result = isNullOrEmpty("\t");   assert(result == true)
    }

    test("`isNullOrEmpty`: handles multiline string") {
        val multilineString =
            """
              |
              |
            """.stripMargin
        val result = isNullOrEmpty(multilineString)
        assert(result == true)
    }

    test("`isNullOrEmpty`: handles non-null and non-blank strings") {
        var result = false
        result = isNullOrEmpty("0");    assert(result == false)
        result = isNullOrEmpty("yo");   assert(result == false)
        result = isNullOrEmpty(nums);   assert(result == false)
        result = isNullOrEmpty(chars);  assert(result == false)
    }


    /**
      * leftTrim
      * --------
      */
    test("`leftTrim`: empty string returns itself") {
        var result = ""
        result = leftTrim("");  assert(result == "")
    }

    test("`leftTrim`: string of blanks returns empty string") {
        var result = ""
        result = leftTrim(" ");  assert(result == "")
        result = leftTrim("  ");  assert(result == "")
        result = leftTrim("   ");  assert(result == "")
    }

    // also: see the property tests for `leftTrim` and `rightTrim`
    test("`leftTrim`: handles blanks on left") {
        var result = ""
        result = leftTrim(" abc");   assert(result == "abc")
        result = leftTrim("  abc");  assert(result == "abc")
    }

    test("`leftTrim`: leaves blanks on right alone") {
        var result = ""
        result = leftTrim(" abc ");   assert(result == "abc ")
        result = leftTrim("  abc  ");  assert(result == "abc  ")
    }

    test("`leftTrim`: handle `\\n` in string") (pending)
    test("`leftTrim`: handle `\\r` in string") (pending)
    test("`leftTrim`: handle `\\t` in string") (pending)



    /**
      * rightTrim
      * --------
      */
    test("`rightTrim`: empty string returns itself") {
        var result = ""
        result = rightTrim("");  assert(result == "")
    }

    test("`rightTrim`: string of blanks returns empty string") {
        var result = ""
        result = rightTrim(" ");  assert(result == "")
        result = rightTrim("  ");  assert(result == "")
        result = rightTrim("   ");  assert(result == "")
    }

    test("`rightTrim`: handles blanks on right") {
        var result = ""
        result = rightTrim("abc ");  assert(result == "abc")
        result = rightTrim("abc  ");  assert(result == "abc")
    }

    test("`rightTrim`: leaves blanks on left alone") {
        var result = ""
        result = rightTrim(" abc ");   assert(result == " abc")
        result = rightTrim("  abc  ");  assert(result == "  abc")
    }

    // also: see the property tests for `leftTrim` and `rightTrim`
    test("`rightTrim`: handle `\\n` in string") (pending)
    test("`rightTrim`: handle `\\r` in string") (pending)
    test("`rightTrim`: handle `\\t` in string") (pending)



    /**
      * capitalizeAllWordsInString
      * --------------------------
      */
    test("`capitalizeAllWordsInString` should handle one word") {
        var result = ""
        result = capitalizeAllWordsInString("a");    assert(result == "A")
        result = capitalizeAllWordsInString("hi");   assert(result == "Hi")
        result = capitalizeAllWordsInString("how");  assert(result == "How")
    }

    test("`capitalizeAllWordsInString` should handle 2+ words") {
        var result = ""
        result = capitalizeAllWordsInString("how to");  assert(result == "How To")
        result = capitalizeAllWordsInString("how to serve");  assert(result == "How To Serve")
        result = capitalizeAllWordsInString("how to serve man");  assert(result == "How To Serve Man")
    }

    test("`capitalizeAllWordsInString`: handle hyphens") {
        var result = ""
        result = capitalizeAllWordsInString("a-b");     assert(result == "A-b")
        result = capitalizeAllWordsInString("foo-bar"); assert(result == "Foo-bar")
    }

    test("`capitalizeAllWordsInString`: no impact on uppercase words") {
        var result = ""
        result = capitalizeAllWordsInString("FOO");     assert(result == "FOO")
        result = capitalizeAllWordsInString("FOO BAR"); assert(result == "FOO BAR")
        result = capitalizeAllWordsInString("FOO-BAR"); assert(result == "FOO-BAR")
    }

    //TODO this test does not pass
    test("`capitalizeAllWordsInString` handles \\t in string") (pending)
//    test("`capitalizeAllWordsInString` handles \\t in string") {
//        var result = ""
//        result = capitalizeAllWordsInString("how\tto")
//        assert(result(0) == 'H')
//        assert(result(1) == 'o')
//        assert(result(2) == 'w')
//        assert(result(3) == '\t')
//        assert(result(4) == 'T')
//        assert(result(5) == 'o')
//    }

    test("`capitalizeAllWordsInString`: handle `\\n` in string") (pending)
    test("`capitalizeAllWordsInString`: handle `\\r` in string") (pending)



    /**
      * getStackTraceAsString
      * ---------------------
      */
    test("getStackTraceAsString: handles basic throwable") {
        val t = new Throwable("foo bar baz")
        val s = getStackTraceAsString(t)
        assert(s.startsWith("java.lang.Throwable: foo bar baz"))
    }



    /**
      * splitCamelCase
      * --------------
      */
    test("splitCamelCase works on FooBarBaz") {
        val s = splitCamelCase("FooBarBaz")
        assert(s.equals("Foo Bar Baz"))
    }

    test("splitCamelCase works on a blank string") {
        val s = splitCamelCase("")
        assert(s.equals(""))
    }

    test("splitCamelCase throws NPE with a null") {
        val e: NullPointerException = intercept[NullPointerException] {
            val s = splitCamelCase(null)
        }
        assert(e.isInstanceOf[NullPointerException])
    }



    /**
      * convertStringWithNewlinesToSeq
      * ------------------------------
      */
    test("convertStringWithNewlinesToSeq works on \"foo\\nbar\\nbaz\"") {
        val rez = convertStringWithNewlinesToSeq("foo\nbar\nbaz")
        assert(rez == Seq("foo", "bar", "baz"))
    }

    test("convertStringWithNewlinesToSeq works on \"foo\\nbar\"") {
        val rez = convertStringWithNewlinesToSeq("foo\nbar")
        assert(rez == Seq("foo", "bar"))
    }

    //TODO should `"foo\nbar\n"` result in `Seq("foo", "bar", "")`?
    test("convertStringWithNewlinesToSeq works on \"foo\\nbar\\n\"") {
        var rez = convertStringWithNewlinesToSeq("foo\nbar\n")
        assert(rez == Seq("foo", "bar"))

        rez = convertStringWithNewlinesToSeq("foo\nbar\n\n\n")
        assert(rez == Seq("foo", "bar"))

        // note how it works when there are spaces before/after newlines
        rez = convertStringWithNewlinesToSeq("foo\nbar\n \n \n ")
        assert(rez == Seq("foo", "bar", " ", " ", " "))
    }

    //TODO should this result in `Seq()`?
    test("convertStringWithNewlinesToSeq works on empty string") {
        val rez = convertStringWithNewlinesToSeq("")
        assert(rez == Seq(""))
    }


    /**
      * convertCamelCaseToUnderscore
      * ----------------------------
      */
    test("convertCamelCaseToUnderscore: basic use cases") {
        var res = ""
        res = convertCamelCaseToUnderscore("fooBar"); assert(res == "foo_bar")
        res = convertCamelCaseToUnderscore("fooBarBaz"); assert(res == "foo_bar_baz")
    }



    /**
      * convertUnderscoreNameToUpperCase
      * --------------------------------
      */
//    test("convertUnderscoreNameToUpperCase: basic use cases") {
//        var res = ""
//        res = convertUnderscoreNameToUpperCase("foo_bar_baz");  assert(res == "FooBarBaz")
//    }



    /**
      * removeTrailingS
      * ---------------
      */
    test("removeTrailingS: basic use cases") {
        var res = ""
        res = removeTrailingS("dollars");  assert(res == "dollar")
    }


    /**
      * randomAlphanumericString
      * ------------------------
      */
    test("randomAlphanumericString: confirm length is correct") {
        var res = ""
        res = randomAlphanumericString(1);  assert(res.length == 1)
        res = randomAlphanumericString(2);  assert(res.length == 2)
        res = randomAlphanumericString(5);  assert(res.length == 5)
    }

    test("genRandomVariableLengthStringWithBlankSpaces tests") (pending)


    /**
     * containsWhitespace
     * ------------------
     */
    test("`containsWhitespace`") {
        var result = false
        result = containsWhitespace("");     assert(result == false)
        result = containsWhitespace("foo");  assert(result == false)

        result = containsWhitespace("a b");  assert(result == true)
        result = containsWhitespace(" a");   assert(result == true)
        result = containsWhitespace("a ");   assert(result == true)
        result = containsWhitespace(" a ");  assert(result == true)
        result = containsWhitespace("a a");  assert(result == true)
        result = containsWhitespace("a a "); assert(result == true)

        result = containsWhitespace("\t"); assert(result == true)
        result = containsWhitespace("a\t"); assert(result == true)
        result = containsWhitespace("\ta"); assert(result == true)
        result = containsWhitespace("a\tb"); assert(result == true)
        result = containsWhitespace("a\t"); assert(result == true)

        result = containsWhitespace("\r"); assert(result == true)
        result = containsWhitespace("a\r"); assert(result == true)
        result = containsWhitespace("\ra"); assert(result == true)
        result = containsWhitespace("a\rb"); assert(result == true)
        result = containsWhitespace("a\r"); assert(result == true)

        result = containsWhitespace("\n"); assert(result == true)
        result = containsWhitespace("a\n"); assert(result == true)
        result = containsWhitespace("\na"); assert(result == true)
        result = containsWhitespace("a\nb"); assert(result == true)
        result = containsWhitespace("a\n"); assert(result == true)
    }



    /**
     * isBlank
     * -------
     */
    test("`isBlank`") {
        var result = false

        result = isBlank("");     assert(result == true)
        result = isBlank(" ");    assert(result == true)
        result = isBlank("   ");  assert(result == true)
        result = isBlank("\t");   assert(result == true)
        result = isBlank(" \t");  assert(result == true)
        result = isBlank("\t ");  assert(result == true)
        result = isBlank("\r");   assert(result == true)
        result = isBlank("\n");   assert(result == true)

        result = isBlank("foo");  assert(result == false)
        result = isBlank("a ");   assert(result == false)
        result = isBlank(" a");   assert(result == false)
        result = isBlank(" a ");  assert(result == false)

    }


    /**
     * isYes
     * -----
     */
    test("`isYes`") {
        var result = false

        result = isYes("y", false);     assert(result == true)
        result = isYes("Y", false);     assert(result == true)
        result = isYes("yes", false);   assert(result == true)
        result = isYes("YES", false);   assert(result == true)
        result = isYes("Yes", false);   assert(result == true)
        result = isYes("YEs", false);   assert(result == true)
        result = isYes("yES", false);   assert(result == true)
        result = isYes("yeS", false);   assert(result == true)

        result = isYes("y ", false);    assert(result == true)
        result = isYes(" y", false);    assert(result == true)
        result = isYes(" y ", false);   assert(result == true)

        result = isYes("n", false);     assert(result == false)
        result = isYes("N", false);     assert(result == false)
        result = isYes("no", false);    assert(result == false)
        result = isYes("NO", false);    assert(result == false)
        result = isYes("No", false);    assert(result == false)
        result = isYes("nO", false);    assert(result == false)

        result = isYes("", false);      assert(result == false)
        result = isYes(" ", false);     assert(result == false)
        result = isYes("", true);       assert(result == true)
        result = isYes(" ", true);      assert(result == true)
        result = isYes("\t", false);    assert(result == false)
        result = isYes("\t", true);     assert(result == true)

        result = isYes("foo", false);   assert(result == false)
        result = isYes("a b", false);   assert(result == false)
    }



    /**
     * booleanAsYOrN
     * -------------
     */
    test("`booleanAsYOrN`") {
        var result = ""
        result = booleanAsYOrN(true);   assert(result == "y")
        result = booleanAsYOrN(false);  assert(result == "n")
    }


}









