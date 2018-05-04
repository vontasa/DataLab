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
        for i in range(len(s)):
            temp = self.extend(s, i, i)
            if (len(temp)>len(best)):
                best = temp
            temp = self.extend(s, i, i+1)
            if (len(temp)>len(best)):
                best = temp
        return best
    
    def extend(self, s, l, r):
        while l>=0 and r<len(s) and s[l]==s[r]:
            l=l-1
            r=r+1
        return(s[l+1:r])
if __name__ == '__main__':

    print("Longest parlindrome is: ",Solution().longestPalindrome('aa'))