class Solution:
    def newton(self, nums1, nums2):
        """
        :type nums1: List[int]
        :type nums2: List[int]
        :rtype: float
        """
        l = nums1+nums2
        l.sort()
        n = len(l)
        print(l)
        if(n==1):
            return(l[0])
        if(n%2==0):
            return((l[int(n/2-1)]+l[int(n/2)])/2)
        else:
            return(l[int(n/2)])

if __name__ == '__main__':