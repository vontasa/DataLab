# -*- coding: utf-8 -*-
"""
Created on Mon Nov 14 10:18:26 2016
Given an array of integers, return indices of the two numbers such that they add up to a specific target.
You may assume that each input would have exactly one solution, and you may not use the same element twice.
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
            for j, vj in enumerate(nums[i+1:]):
                if ((vi+vj) == target) :
                    result = [i,j+i+1]
                    return result
        
        
def main():
    solution = Solution()
    print(solution.twoSum([3,2,4],6))
    print(solution.twoSum([1,2,3,4,5,6],11))
    
if __name__ == "__main__":
    main()