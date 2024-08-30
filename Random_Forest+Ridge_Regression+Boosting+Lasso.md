Random Forest + Ridge Regression + Boosting + Lasso
================

``` r
#regression model
winequality <- read.csv('winequality-red.csv', header = T, sep = ";")

#linear model
winelm <- lm(quality~.,data=winequality)
ws<-summary(winelm)
ws
```

    ## 
    ## Call:
    ## lm(formula = quality ~ ., data = winequality)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.68911 -0.36652 -0.04699  0.45202  2.02498 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           2.197e+01  2.119e+01   1.036   0.3002    
    ## fixed.acidity         2.499e-02  2.595e-02   0.963   0.3357    
    ## volatile.acidity     -1.084e+00  1.211e-01  -8.948  < 2e-16 ***
    ## citric.acid          -1.826e-01  1.472e-01  -1.240   0.2150    
    ## residual.sugar        1.633e-02  1.500e-02   1.089   0.2765    
    ## chlorides            -1.874e+00  4.193e-01  -4.470 8.37e-06 ***
    ## free.sulfur.dioxide   4.361e-03  2.171e-03   2.009   0.0447 *  
    ## total.sulfur.dioxide -3.265e-03  7.287e-04  -4.480 8.00e-06 ***
    ## density              -1.788e+01  2.163e+01  -0.827   0.4086    
    ## pH                   -4.137e-01  1.916e-01  -2.159   0.0310 *  
    ## sulphates             9.163e-01  1.143e-01   8.014 2.13e-15 ***
    ## alcohol               2.762e-01  2.648e-02  10.429  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.648 on 1587 degrees of freedom
    ## Multiple R-squared:  0.3606, Adjusted R-squared:  0.3561 
    ## F-statistic: 81.35 on 11 and 1587 DF,  p-value: < 2.2e-16

``` r
mean(ws$residuals^2)
```

    ## [1] 0.4167672

``` r
#MSE:0.4167
```

``` r
#tree
library(tree)
winetree <- tree(quality~., data=winequality)

plot(winetree)
text(winetree)
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
cv.winequalityt <- cv.tree (winetree, FUN = prune.tree)
plot(cv.winequalityt, type="b")
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
#no pruning

trainindex <- sample(1: nrow(winequality),800)
winetrain <- winequality[trainindex,]
winetest <- winequality[-trainindex,]

yhat <- predict(winetree,winetest[,-12])
mean((yhat - winetest[,12])^2)
```

    ## [1] 0.4183862

``` r
#MSE:0.4108023
```

``` r
#randomForest
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
wineRF <- randomForest(quality~., data=winequality, mtry=2,importance=TRUE)
wineRF
```

    ## 
    ## Call:
    ##  randomForest(formula = quality ~ ., data = winequality, mtry = 2,      importance = TRUE) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##           Mean of squared residuals: 0.3214148
    ##                     % Var explained: 50.69

``` r
#MSE:0.3191
```

``` r
#boosting with loovc
attach(winequality)
library(gbm)
```

    ## Loaded gbm 2.1.4

``` r
winequalityboost <- gbm(quality~., distribution="gaussian", data=winequality, n.trees=5000, interaction.depth=1)

cvboost <- NA
for(i in 1:nrow(winequality))
  {
dummod <- gbm(quality~., distribution="gaussian", data=winequality[-i,], n.trees=5000, interaction.depth=1)

cvboost[i] <- ((predict(dummod, n.trees=5000, newdata=winequality[i,], type="response"))- quality[i]) ^2 
}

mean(cvboost)
```

    ## [1] 0.4291804

``` r
#MSE:0.434
```

``` r
#Lasso
#install.packages("glmnet")
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-16

``` r
grid <- exp(seq(10, -6, length=100))
x <- as.matrix(winequality[,-12])
y <- winequality$quality
lasim <- cv.glmnet(x, y, alpha=1,lambda=grid)

plot(lasim$glmnet.fit, label=TRUE, xvar="lambda")
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(lasim)
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
lammin <- lasim$lambda.min
lam1se <- lasim$lambda.1se
lammin
```

    ## [1] 0.007683448

``` r
mse <- lasim$cvm[lasim$lambda ==lasim$lambda.min]
mse
```

    ## [1] 0.4247248

``` r
#MSE: 0.424354
```

``` r
#ridge regression
rrsim_d <- cv.glmnet(x, y, alpha=0)
plot(rrsim_d$glmnet.fit, label=TRUE, xvar="lambda")
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
plot(rrsim_d)
```

![](Random_Forest+Ridge_Regression+Boosting+Lasso_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
mse_r <- rrsim_d$cvm[rrsim_d$lambda ==rrsim_d$lambda.min]
mse_r
```

    ## [1] 0.4236019

``` r
#MSE: 0.4241866
```
