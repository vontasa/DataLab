# -*- coding: utf-8 -*-
"""
Created on Tue May  8 11:25:51 2018

@author: edward.wang

Implement atoi which converts a string to an integer.

The function first discards as many whitespace characters as necessary until the first non-whitespace character is found. Then, starting from this character, takes an optional initial plus or minus sign followed by as many numerical digits as possible, and interprets them as a numerical value.

The string can contain additional characters after those that form the integral number, which are ignored and have no effect on the behavior of this function.

If the first sequence of non-whitespace characters in str is not a valid integral number, or if no such sequence exists because either str is empty or it contains only whitespace characters, no conversion is performed.

If no valid conversion could be performed, a zero value is returned.
"""
import re
class Solution:
    def myAtoi(self, str):
        """
        :type str: str
        :rtype: int
        """
        # revmoe the leading space
        str1 = re.sub(r'^[ \s]+', '', str)
        print('str1:', str1)
        
        # remove the end letters
        str2 = re.sub(r'[^0-9]+$', '', str1)
        print('str2:', str2)

    # start with non - or number
        if(re.search(r'^[^\+\-0-9]', str2)):
            return 0
        # Search the number start as +-or number, til the first non number being found
        if(re.search(r'^[\+\-0-9]+?(?=[^0-9])', str2)):
            str2 = re.search(r'^[\+\-0-9]+?(?=[^0-9])', str2).group(0)

        print('Pure integar: ',str2)

        if(len(str2)==0):
            return 0   

        if(str2[0]=='+'):
            if(len(str2)==1):
                return 0
            str2 = str2[1:]
            
        if(str2[0]=='-'):
            if(len(str2)==1):
                return 0


        res = max(int(str2), -2**31)
        res = min(res, 2**31-1)
        return res
            
if __name__ == '__main__':
    print("number is: ",Solution().myAtoi('   s23231')) 