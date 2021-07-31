Distribution of p-value
================
2DoLLars

A p-value is the probability of obtaining test results at least as
extreme as the results actually observed, under the assumption that the
null hypothesis is correct. Rejection the null hypothesis if p-value is
less than 0 ≤ *α* ≤ 1 coincides with statistical testing with level *α*.
It is due to the distribution of p-value is uniform(0, 1) under the null
hypothesis. In this page, we empirically check the distribution of
p-value via iterative simulation,

# setting

``` r
iter = 10^5
n = 100
mu = 5
sd = 1
```

# result

By iteration with 10<sup>5</sup> times, we get the observations of
p-value. The histogram of the observations of p-value looks uniformly
distributed on the interval \[0, 1\]

``` r
result = rep(NA, iter)
for(i in 1:iter)
{
   set.seed(i)
   x = rnorm(n, mean = mu, sd = sd)
   se = sd(x) / sqrt(n)
   t = ( mean(x) - mu ) / se
   prob_get_extream = 1 - pt(t, df = n - 1)
   result[i] = prob_get_extream
}

hist(result, probability = TRUE, xlab = "p-value", main = "Histogram of p-value")
```

![Histogram](C:/Users/User/Desktop/github코드/distribution_p_value_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Komogorov-Smirnov test

We check that KS test supports the desired result

``` r
ks.test(result, runif(n))
```

    ## 
    ##  Two-sample Kolmogorov-Smirnov test
    ## 
    ## data:  result and runif(n)
    ## D = 0.11015, p-value = 0.177
    ## alternative hypothesis: two-sided
