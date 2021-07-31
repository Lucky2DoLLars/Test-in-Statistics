Significance level test
================
2DoLLars

A significance level is the probability of the study rejecting the null
hypothesis, given that the null hypothesis was assumed to be true. We
use significance level to set criteria for statistical hypothesis
testing. In this file, we understand empirically the meaning of
significance level via simulation for simple T-test.

# setup

We iterate generating sample 10^5 times. A significance level is set by
0.05

``` r
iter = 10^5
n = 100
mu = 5
sd = 1
alpha = 0.05
t_value = qt(1 - alpha, df = n - 1)
```

# result

For each generated sample, we check that sample is in rejection area. As
a result, we find that the ratio of rejection is the almost same as
sginficance level

``` r
result = rep(NA, iter)
for(i in 1:iter)
{
   set.seed(i)
   x = rnorm(n, mean = mu, sd = sd)
   se = sd(x) / sqrt(n)
   t = (mean(x) - 5) / se
   result[i] = (t > t_value)
}

print(paste0("The ratio of rejcection : ", mean(result)))
```

    ## [1] "The ratio of rejcection : 0.04998"
