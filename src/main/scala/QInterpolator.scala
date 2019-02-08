package com.alvinalexander.utils

object QInterpolator {

    implicit class QHelper(val sc: StringContext) {
        def Q(expressions: Any*): Seq[String] = {

            // for my purpose i can just put the original string back
            // together by calling the `s` interpolator
            val originalString: String = sc.s(expressions: _*)

            originalString.toString.split("\n")
                .toVector
                .map(_.trim)
                .filter(_ != "")

        }
    }

}

