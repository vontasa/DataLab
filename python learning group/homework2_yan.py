 #!/usr/bin/python2.7.9
import numpy as np
from scipy import stats

# alternative = ['greater','less','two-sided']
def t_test(a,mu, alternative='greater', significance=0.05):
    mean_a = np.mean(a)
    stdev_a = np.std(a) # sample std
    ub_std = stdev_a * np.sqrt(len(a)/(len(a)-1)) # un-biased std from sample
    se = ub_std/np.sqrt(len(a)) # standard error
    df=float(len(a)-1) # degree of freedom
    t = (mean_a-mu)/se
    if alternative=='two-sided':
        prob = stats.t.sf(np.abs(t), float(df))*2  # use np.abs to get upper tail
        cv = 1-significance/2
        ts = stats.t.ppf(significance/2, df)
        ci_lower = np.mean(a)-se*ts
        ci_upper = np.mean(a)+se*ts
    elif alternative=='greater':
        prob = stats.t.sf(t, float(df))
        cv = 1-significance
        ts = stats.t.ppf(significance, df)
        ci_lower = float("-inf")
        ci_upper = np.mean(a)+se*ts
    else:
        prob = stats.t.sf(-t, float(df))
        cv = 1-significance
        ts = stats.t.ppf(significance, df)
        ci_lower = np.mean(a)-se*ts
        ci_upper = float("inf")
    return t, cv, prob, ci_lower, ci_upper

x=np.random.normal(2.5,1.5, 64) # mean is 2.5 hours. The sample standard deviation is 1.5 hours. 64 managers
print(np.mean(x))
print t_test(x,mu=3, alternative='two-sided')
