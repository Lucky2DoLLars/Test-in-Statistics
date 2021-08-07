Contours of RSS with multicolinearity
================
2DoLLars

In statistics, multicollinearity is a phenomenon in which one predictor
variable in a multiple regression model can be linearly predicted from
the others with a substantial degree of accuracy. With this phenomenon,
it is known that the computation of regression coefficients is unstable.
We plot the contours of RSS for different degree of multicollinearity to
see its effect.

## RSS function

``` r
rm(list = ls())
library(plot3D)
RSS = function(beta1, beta2)
{
   beta = c(beta1, beta2)
   RSS = sum((y - x %*% beta)^2)
   return(RSS)
}
```

## contours of RSS

We generate 3 different synthetic data set having different degree of
correlation between variables, then plot the contours of RSS for each.

``` r
par(mfrow = c(1, 3))
for(sd in c(0.01, 0.5, 1.3))
{
   set.seed(3)
   ngrid = 200
   z1 = seq(-2, 4, length = ngrid)
   z2 = seq(-2, 4, length = ngrid)
   
   x1 = rnorm(10)
   x2 = x1 + rnorm(10, 0, sd = sd)
   y = x1 + x2 + rnorm(10, sd = 0.3)
   
   x = cbind(x1, x2)
   x_grid = expand.grid(z1, z2)
   
   RSS_v = rep(NA, length = ngrid^2)
   for(i in 1 : length(RSS_v))
   {
      RSS_v[i] = RSS(x_grid[i, 1], x_grid[i, 2])
   }

   r = round(cor(x1, x2), 2)
   title = paste("corr :", r)
   contour(z1, z2, matrix(RSS_v, ngrid, ngrid), col = "blue", nlevels = 50, main = title,
           xlab = bquote(~beta[1]), ylab = bquote(~beta[2]))
}
```

![](/image/multicollinearity_contours.png)<!-- -->
