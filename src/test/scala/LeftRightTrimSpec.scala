package com.alvinalexander.utils

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * ScalaCheck specifications/rules for `leftTrim` and `rightTrim`.
  */
object LeftRightTrimSpec extends Properties("LeftRightTrimSpec") {

    // see https://www.scala-exercises.org/scalacheck/generators
    // see https://booksites.artima.com/scalacheck/examples/html/ch06.html
    object GenString {

        val r = new scala.util.Random
        import StringUtils.genRandomVariableLengthStringWithBlankSpaces

        //TODO hack; need a better way to go from String -> Gen[String]
        def stringWithManyBlanks: Gen[String] = Gen.oneOf(
            genRandomVariableLengthStringWithBlankSpaces(r),
            genRandomVariableLengthStringWithBlankSpaces(r)
        )

        /**
          * Ideas below here
          * ----------------
          */
        val threeLetters: Gen[Seq[Char]] = Gen.pick(3, 'A' to 'Z')
        val genStringStream = Gen.containerOf[Stream,String](Gen.alphaStr)
        val evenInteger = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 0)
        def badStateGen: Gen[String] = Gen.choose(73, 99).toString
        val genNonEmptyString: Gen[String] = Gen.alphaStr.suchThat(i => !i.isEmpty)
        val genNonEmptyNumString: Gen[String] = Gen.numStr.suchThat(i => !i.isEmpty)

        // generates a string of ASCII characters, with extra weighting for printable characters.
        // bad: generates characters that make the terminal beep.
        val asciiStr = Gen.asciiStr
        // generates a string of ASCII printable characters.
        // this is better, but it doesn’t generate many blanks.
        val asciiPrintableStr = Gen.asciiPrintableStr

    }

    // let ScalaCheck generate any sort of string
    property("leftTrim") = forAll { s: String =>
        val result = StringUtils.leftTrim(s)
        !result.startsWith(" ")
    }

    /**
      * generate our own strings with plenty of blank spaces.
      * rule: no matter what string you give to `leftTrim`, it should never
      * begin with a blank space.
      */
    //property("dropAllButFirstIntLists") = forAll(Gen.alphaStr) { s: String =>
    property("leftTrimNeverBeginsWithBlankSpace") = forAll(GenString.stringWithManyBlanks) { s: String =>
        //println(s)
        val result = StringUtils.leftTrim(s)
        !result.startsWith(" ")
    }

    property("rightTrimNeverEndsWithBlankSpace") = forAll(GenString.stringWithManyBlanks) { s: String =>
        val result = StringUtils.rightTrim(s)
        !result.endsWith(" ")
    }


}


