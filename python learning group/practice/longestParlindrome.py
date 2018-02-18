class Solution:
    def longestPalindrome(self, s):
        """
        :type s: str
        :rtype: str
        Input: "babad"
        Output: "bab"
        Note: "aba" is also a valid answer.
        """

        best = ''
        bestLen = 0
        for i in range(len(s)):
            for j in range(0,min(i, len(s)-i)+1):
                print("center is", s[i], sep=' ')
                sub = s[(i-j):(i+j+1)]
                # Reverse the string
                if(sub == sub[::-1]):
                    print("Got a parlindrome:", sub, sep=' ')
                    if(len(sub)> bestLen):
                        # Update the best result
                        bestLen = len(sub)
                        best = sub
                        print('best sub is:', best, "best len is:", bestLen, sep=' ')
        return best
if __name__ == '__main__':

    print("Longest parlindrome is: ",Solution().longestPalindrome('kduwdcabacdad'))