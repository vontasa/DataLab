# -*- coding: utf-8 -*-
"""
Created on Mon Nov 14 10:18:26 2016

@author: edward.wang
"""

class Solution(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        result = []
        for i, vi in enumerate(nums):
            for j, vj in enumerate(nums):
                if (vi+vj)==target:
                    result = [i,j]
                    return result
        
        
def main():
    solution = Solution()
    print solution.twoSum([3,2,4],6)
    
if __name__ == "__main__":
    main()