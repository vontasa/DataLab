# -*- coding: utf-8 -*-
"""
Created on Fri Mar 23 11:24:00 2018
Reverse interger
input 123
output 321

input -123
output -321

input 120
output 21

Assume we are dealing with an environment which could only hold integers within the 32-bit signed integer range. For the purpose of this problem, assume that your function returns 0 when the reversed integer overflows.
@author: edward.wang
"""

class Solution:
    def reverse(self, x):
        """
        :type x: int
        :rtype: int
        """
        x_str = str(abs(x))
        y_str = ''
        for i, v in enumerate(x_str):
            y_str = v + y_str
            
        y = int(y_str)
        if(x<0):
            y = -1*y
            
        if(abs(y) > (2**31-1)):
            y= 0
            
        return(y)

if __name__ == '__main__':
    solution = Solution()
    print(solution.reverse(123))
    print(solution.reverse(-123))
    print(solution.reverse(1563847412))
    print(solution.reverse(1534236469))