# -*- coding: utf-8 -*-
"""
Created on Tue Nov 15 10:46:31 2016

@author: edward.wang
@description: 
   
"""

class Solution(object):
    """
    Implement strStr().
    Returns the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.
    Subscribe to see which companies asked this question
    """
    def strStr(self, haystack, needle):
        """
        :type haystack: str
        :type needle: str
        :rtype: int
        """
        result = haystack.find(needle)
        return result
        
    """
    Given an unsorted integer array, find the first missing positive integer.
    For example,
    Given [1,2,0] return 3,
    and [3,4,-1,1] return 2.
    Your algorithm should run in O(n) time and uses constant space.
    """
    def firstMissingPositive(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        n = len(nums)
        if n > 0:
            map ={}
            for i in nums:
                map[str(i)] = i
            for i in range(1, n+1):
                if (str(i) not in map):
                    return i
            return n+1
        else:
            return 1

    """
    Given a binary tree, return the level order traversal of its nodes' values. (ie, from left to right, level by level).
    For example:
        Given binary tree [3,9,20,null,null,15,7],
    """
    def levelOrder(self, root):
        """
        :type root: TreeNode
        :rtype: List[List[int]]
        """
        N = len(root)
        l = 0
        tree = []
        while 2**l<=N :
            start = 2**l-1
            end = min(2**(l+1)-1, N)
            tree.append(root[start : end])
            l+=1
        return tree
        
def main():
    solution = Solution()
#    print solution.strStr("123","a")
#    print solution.firstMissingPositive([1])
#    print solution.levelOrder([1,2,3,4,5,6,7,8,9,0])
    
if __name__ == "__main__":
    main()