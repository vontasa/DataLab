# Python Learning Group #

## Member Sign-in ##

If you want to join the Python Learning Group, please sign your name here and answer one question below.
Question: Use one sentence to describe why you want to join this group?

Current member
Edward: I want to be a full-stack data scientist.
Yue: I want to refresh my knowledge in Python and doing it by myself is lonely.
Jialin: I want to learn more about using python for data mining purpose.

__*You sign up here*__


## Homework

### Homework 1
1. Sign-up yourself in this readme file.
1. Commit the change as a new branch

### Homework 2
Try to write your own user-defined function for t testing, similar to the t.test() function used in R (https://stat.ethz.ch/R-manual/R-devel/library/stats/html/t.test.html)

1.Input:

<code>t_test(mean, stdev, mu, alternative, significance level)</code>

* mean: sample average of data
* stdev: sample/population standard deviation
* mu: a number indicating the true value of the mean
* alternative: “two.sided”, “less” or “greater” corresponding to one-sided test
* significance level: alpha

If our data sample size is large (>30), we could use standard normal distribution for critical value. So let’s use normal distribution for critical value or p-value.

2. Output: Print t-statistic, critical value, p-value and confidence interval.

3. Apply your function to the following example:
An analyst is conducting a hypothesis test to determine if the mean time spent on investment research is different from three hours per day. The test is performed at the 5% level of significance and uses a random sample of 64 portfolio managers, where the mean time spent on research is found to be 2.5 hours. The sample standard deviation is 1.5 hours. The analyst should most appropriately reject or fail to reject the null hypothesis: mu = 3 hours?
