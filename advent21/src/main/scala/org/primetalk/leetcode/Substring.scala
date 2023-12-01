package org.primetalk.leetcode

/**
 * Для заданной строки найти длину наибольшей подстроки без повторяющихся символов.
 * Например:
 * abcabcbbddee -> 3 (abc)
 * bbbbb -> 1 (b)
 * pwwkew -> 3 (wke)
 *
 */
object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
        def loop(start: Int = 0, finish1: Int = 0, chars: Set[Char] = Set(), historyMaxSize: Int = 0): Int = {
            if finish1 == s.length then
              math.max(historyMaxSize, chars.size)
            else
              val c = s(finish1)
              if chars.contains(c) then
                //loop(start)
                def advanceStart(pos: Int = start, chars: Set[Char] = chars): (Int, Set[Char]) =
                    if s(pos) == c then 
                        (pos + 1, chars - s(pos))
                    else
                        advanceStart(pos + 1, chars - s(pos))
                val (newStart, newChars) = advanceStart()
                loop(newStart, finish1 + 1, newChars + c, math.max(historyMaxSize, chars.size))
              else
                loop(start, finish1 + 1, chars + c, historyMaxSize)
        }
        loop()
    }
}




