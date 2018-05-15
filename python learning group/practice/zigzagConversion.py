# -*- coding: utf-8 -*-
"""
Created on Fri May  4 16:53:18 2018

@author: edward.wang
"""

class Solution:
    def convert(self, s, numRows):
        """
        :type s: str
        :type numRows: int
        :rtype: str
        """
        if numRows == 1 or numRows >= len(s):
            return s
        
        row, direction, res = 0, -1, ['']*numRows
        for char in s:
            # append the char on the end of each row
            res[row] += char
            # Change the direction when hit the boundary
            if row == 0 or row == numRows-1:
                direction *= -1
            row += direction
        return ''.join(res)
    
if __name__ == '__main__':
    print("zig zag print: ",Solution().convert('1234567', 3))        
        