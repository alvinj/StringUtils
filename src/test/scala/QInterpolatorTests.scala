package com.alvinalexander.utils

import org.scalatest.FunSuite
import com.alvinalexander.utils.QInterpolator._

class QInterpolatorTests extends FunSuite {

    test("`Q` works on one-item list") {
        var list = Q"""
                foo
            """
        assert(list == Vector("foo"))

        list = Q"bar"
        assert(list == Vector("bar"))

        list = Q"   baz   "
        assert(list == Vector("baz"))
    }

    test("`Q` works on multi-item list") {
        val list = Q"""
                apples
                bananas
                cherries
            """
        assert(list == Vector("apples", "bananas", "cherries"))
    }

    test("`Q`: empty string returns empty list") {
        val emptyVector = Vector[String]()

        var list = Q"";     assert(list == emptyVector)
        list = Q" \n\r\t";  assert(list == emptyVector)
        list = Q"  ";       assert(list == emptyVector)

        list = Q"""
               """
        assert(list == emptyVector)

        list = Q"""

               """
        assert(list == emptyVector)
    }

    test("`Q` works on multiline string with multi-words per line") {
        val list = Q"""
                apples and
                bananas and
                cherries the end
            """
        assert(list == Vector("apples and", "bananas and", "cherries the end"))
    }

}






