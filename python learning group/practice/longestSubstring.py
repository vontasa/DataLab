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
        head=0
        tail=0
        bestH = 0
        bestT = 0
        for i in range(0,len(s)):   
            v=s[i]          
            if v in d:
                if(d[v] >= head):
                    current=i-head
                    if(current > best):
                        best = current
                        bestH = head
                        bestT = i-1
                        bestSub = s[head:i]
                    # Update the dictionary and head
                    head=d[v]+1
                else:
                    current=i-head+1
            else:
                current=i-head+1
            d[v]=i
        # For the case best string is from head to end
        best = max(current, best)
        return best
        
if __name__ == '__main__':
    
    #print(Solution().lengthOfLongestSubstring("tmmzuxt")==5)
    #print(Solution().lengthOfLongestSubstring("qwnfenpglqdq"))
    print(Solution().lengthOfLongestSubstring("q"))
        