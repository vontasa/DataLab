class Solution:
    def lengthOfLongestSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        bestSub=[]
        best=0
        current=0
        sub=[]
        d={}
        for i, v in s:
            sub=s[i:len(s)]
            d={}
            curr=0
            for j,vj in sub:
                if d[v]:
                    if curr>best:
                        best=curr
                    break
                else:
                    d[v]=1
                    curr+=1
        return best
        
if __name__ == '__main__':
    result=Solution.lengthOfLongestSubstring("sdsdssfjjhuinawdnk")
        