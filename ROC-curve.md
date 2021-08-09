ROC curve
================
2DoLLars

ROC(Receiver Operating Characteristic) curve is a graphical plot that
illustrates the diagnostic ability of a binary classifier system as its
discrimination threshold is varied. We draw ROC curve manually for the
logistic regression model using `Smarket` data in `ISLR` library.

## Load data and fitting logistic regression model

``` r
# install.packages("ISLR")
library(ISLR)
data(Smarket)

glm.fit = glm(Direction ~ Lag1 + Lag2+ Lag3 + Lag4 + Lag5 + Volume , family = "binomial", data = Smarket)
contrasts(Smarket$Direction)
```

    ##      Up
    ## Down  0
    ## Up    1

## Confusion matrix with threshold = 0.5

``` r
glm.probs = predict(glm.fit, type ="response")
glm.pred = rep("Down", nrow(Smarket))
threshold = 0.5
glm.pred[glm.probs > threshold] = "Up"
t(table(glm.pred, Smarket$Direction))
```

    ##       glm.pred
    ##        Down  Up
    ##   Down  145 457
    ##   Up    141 507

## Drawing ROC curve

We draw ROC curve by getting True positive rate and False positive rate
at

``` r
threshold = seq(0, 1, length.out = 1000)
TPR_v = rep(NA, 1000)
FPR_v = rep(NA, 1000)

for (i in 1 : length(threshold))
{
   glm.pred = rep("Down", nrow(Smarket))
   glm.pred[glm.probs > threshold[i]] = "Up"
   glm.pred = factor(glm.pred, levels = c("Down", "Up"))
   table = t(table(glm.pred, Smarket$Direction))
   TPR_v[i] = table[2, 2] / sum(Smarket$Direction == "Up")
   FPR_v[i] = table[1, 2] / sum(Smarket$Direction == "Down")
}

par(mfrow = c(1,1))
plot(NA, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False positive rate",
     ylab = "True positive rate", main = "ROC curve")

x.grid = sort(FPR_v)
y.grid = sort(TPR_v)

lines(x = x.grid, y = y.grid, col = "red", type = "s")

abline(a = 0, b = 1, col = "blue")
```

![](/image/ROC_curve.png)<!-- -->
