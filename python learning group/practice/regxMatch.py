class Solution:
    def isMatch(self, s, p):
        """
        :type s: str
        :type p: str
        :rtype: bool

        '.' Matches any single character.
        '*' Matches zero or more of the preceding element.

        Some examples:
            isMatch("aa","a") → false
            isMatch("aa","aa") → true
            isMatch("aaa","aa") → false
            isMatch("aa", "a*") → true
            isMatch("aa", ".*") → true
            isMatch("ab", ".*") → true
            isMatch("aab", "c*a*b") → true
        """
        if not p:
            return (not s)



if __name__ == '__main__':