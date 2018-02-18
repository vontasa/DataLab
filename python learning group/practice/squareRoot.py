class Solution:
    def newton(self, n, eps = 0.0001, x0=1):
        """
        :type n: int
        Solve the square root of n^0.5 by using newton method
        fx = x^2-n
        f'x = 2x
        xn+1 = xn-fx/f'x
        """
        e=10
        x=x0
        i=0
        while(e>eps or i<1000):
            x = float(x- (x**2-n)/(2*x))
            e = x**2-n
            i=i+1
        return(x)
    
    def fastInverse(self, n):
        import numpy as np
        threehalfs = 1.5
        x2 = n * 0.5
        y = np.float32(n)
        
        i = y.view(np.int32)
        i = np.int32(0x5f3759df) - np.int32(i >> 1)
        y = i.view(np.float32)
        
        y = y * (threehalfs - (x2 * y * y))
        return float(1/y)
        
    def biparty(self, n,start =0, end=0, E=0.001, N=1000):
        if((start+end)==0):
            end = n
        x = (start+end)/2
        e = x**2-n
        if(abs(e)<E):
            return x
        elif e>0: # result in the left half
            end = x
        else: # result in the right half
            start = x
        return (self.biparty(n, start = start, end = end))
        


if __name__ == '__main__':
    print(Solution().newton(2))
    print(Solution().newton(2, x0=-1))
    print(Solution().fastInverse(2))
    print(Solution().biparty(3))