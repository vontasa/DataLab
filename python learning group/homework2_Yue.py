# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
from __future__ import division
from collections import Counter
from matplotlib import pyplot as plt
import random, math
def bernoulli_trial(p):
    return 1 if random.random() < p else 0

def binomial(n, p):
    return sum(bernoulli_trial(p) for _ in range(n))

def poisson_pdf(mu, k):
    return (mu ** k) * math.exp(- mu) / math.factorial(k)

def make_hist(p, n, num_points):

    data = [binomial(n, p) for _ in range(num_points)]
    histogram = Counter(data)
    plt.bar([x - 0.4 for x in histogram.keys()],
            [v/num_points for v in histogram.values()],
            0.8,
            color = '0.75')

    mu = n * p

    xs = range(min(data), max(data) + 1)
    ys = [poisson_pdf(mu, i) for i in xs]
    plt.plot(xs, ys)
    plt.title("Binomial Distribution vs. Poisson Approximation")
    plt.show()

def normal_cdf(x, mu=0,sigma=1):
    return (1 + math.erf((x - mu) / math.sqrt(2) / sigma)) / 2

def inverse_normal_cdf(p, mu=0, sigma=1, tolerance=0.00001):
    """find approximate inverse using binary search"""

    # if not standard, compute standard and rescale
    if mu != 0 or sigma != 1:
        return mu + sigma * inverse_normal_cdf(p, tolerance=tolerance)

    low_z, low_p = -10.0, 0            # normal_cdf(-10) is (very close to) 0
    hi_z,  hi_p  =  10.0, 1            # normal_cdf(10)  is (very close to) 1
    while hi_z - low_z > tolerance:
        mid_z = (low_z + hi_z) / 2     # consider the midpoint
        mid_p = normal_cdf(mid_z)      # and the cdf's value there
        if mid_p < p:
            # midpoint is still too low, search above it
            low_z, low_p = mid_z, mid_p
        elif mid_p > p:
            # midpoint is still too high, search below it
            hi_z, hi_p = mid_z, mid_p
        else:
            break

    return mid_z



def t_test(mean, stdev, mu, n, alternative, significance):
    t_stat = (mean-mu)/(stdev/math.sqrt(n))
    print("t statistic: {0:.3f}".format(t_stat))
    Flag = 0 #fail to reject
    if alternative == "two-sided":
        t_crit = inverse_normal_cdf(1-significance/2)
        if abs(t_stat) > t_crit:
            Flag = 1
        if mean >= mu:
            p_value = 2 * (1 -  normal_cdf(t_stat))
        else:
            p_value = 2 * normal_cdf(t_stat)
    if alternative == "greater":
        t_crit = inverse_normal_cdf(1-significance)
        if t_stat > t_crit:
            Flag = 1
        p_value = 1 - normal_cdf(t_stat)
    if alternative == "less":
        t_crit = inverse_normal_cdf(significance)
        if t_stat < t_crit:
            Flag = 1
        p_value = normal_cdf(t_stat)
    print("Critical value: {0:.3f}".format(t_crit))
    print("P-value: {0:.3f}".format(p_value))
    if Flag == 0:
        print("Fail to reject null hypothesis")
    else:
        print("Reject null hypothesis")
    hw = inverse_normal_cdf(1-significance/2)*stdev/math.sqrt(n)
    ci_low = mean-hw
    ci_high = mean+hw
    print "Confidence interval: [{0:.3f}".format(ci_low), "{0:.3f}]".format(ci_high)

#make_hist(0.01, 100, 100000)
t_test(2.5,1.5,3,64,"two-sided",0.05 )
