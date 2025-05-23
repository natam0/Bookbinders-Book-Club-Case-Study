Bookbinders Book Club Mail Program
================
2025-02-18

``` r
library(readxl)
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(lattice)
library(ggplot2)
library(logistf)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ purrr::lift()   masks caret::lift()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(ROCR)
library(e1071)
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.
    ## 
    ## Attaching package: 'pROC'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

Read in train and test sets and drop `Observation`

``` r
bbbctrain = read_excel('BBBC-Train.xlsx')
bbbctrain = bbbctrain[,-1]
```

``` r
bbbctest = read_excel('BBBC-Test.xlsx')
bbbctest = bbbctest[,-1]
```

Checking for missing variables

``` r
anyNA(bbbctest)
```

    ## [1] FALSE

``` r
anyNA(bbbctrain)
```

    ## [1] FALSE

Checking for correlated variables

``` r
cor(bbbctrain)
```

    ##                        Choice       Gender Amount_purchased     Frequency
    ## Choice            1.000000000 -0.141558415       0.11815256 -0.2260181193
    ## Gender           -0.141558415  1.000000000      -0.03060700  0.0321704951
    ## Amount_purchased  0.118152563 -0.030607000       1.00000000  0.0136664846
    ## Frequency        -0.226018119  0.032170495       0.01366648  1.0000000000
    ## Last_purchase     0.141437015 -0.028963412       0.44070127 -0.0419432803
    ## First_purchase    0.003157481  0.001026138       0.37481393  0.4459457457
    ## P_Child           0.008523377 -0.041475936       0.29931372 -0.0433279437
    ## P_Youth           0.027608101 -0.014130306       0.18755727 -0.0095854745
    ## P_Cook           -0.040256351 -0.026673876       0.30425340  0.0004968833
    ## P_DIY            -0.005309265 -0.025946174       0.22331539 -0.0089634125
    ## P_Art             0.357688817 -0.003500037       0.27248948 -0.0613754066
    ##                  Last_purchase First_purchase      P_Child      P_Youth
    ## Choice              0.14143702    0.003157481  0.008523377  0.027608101
    ## Gender             -0.02896341    0.001026138 -0.041475936 -0.014130306
    ## Amount_purchased    0.44070127    0.374813928  0.299313719  0.187557270
    ## Frequency          -0.04194328    0.445945746 -0.043327944 -0.009585474
    ## Last_purchase       1.00000000    0.814674687  0.679133923  0.453258910
    ## First_purchase      0.81467469    1.000000000  0.544820825  0.367892128
    ## P_Child             0.67913392    0.544820825  1.000000000  0.174826719
    ## P_Youth             0.45325891    0.367892128  0.174826719  1.000000000
    ## P_Cook              0.67250539    0.571054792  0.294706519  0.181656640
    ## P_DIY               0.55816739    0.462018843  0.253837077  0.188683456
    ## P_Art               0.53433415    0.442082061  0.224512850  0.141751220
    ##                         P_Cook        P_DIY        P_Art
    ## Choice           -0.0402563507 -0.005309265  0.357688817
    ## Gender           -0.0266738763 -0.025946174 -0.003500037
    ## Amount_purchased  0.3042533969  0.223315392  0.272489483
    ## Frequency         0.0004968833 -0.008963412 -0.061375407
    ## Last_purchase     0.6725053933  0.558167395  0.534334145
    ## First_purchase    0.5710547918  0.462018843  0.442082061
    ## P_Child           0.2947065185  0.253837077  0.224512850
    ## P_Youth           0.1816566401  0.188683456  0.141751220
    ## P_Cook            1.0000000000  0.271725126  0.191680761
    ## P_DIY             0.2717251256  1.000000000  0.207791065
    ## P_Art             0.1916807611  0.207791065  1.000000000

Drop `First_purchase` because it was highly correlated with
`Last_purchase` and check for correlation again

``` r
bbbctrain|>
  select(-First_purchase)|>
  cor()
```

    ##                        Choice       Gender Amount_purchased     Frequency
    ## Choice            1.000000000 -0.141558415       0.11815256 -0.2260181193
    ## Gender           -0.141558415  1.000000000      -0.03060700  0.0321704951
    ## Amount_purchased  0.118152563 -0.030607000       1.00000000  0.0136664846
    ## Frequency        -0.226018119  0.032170495       0.01366648  1.0000000000
    ## Last_purchase     0.141437015 -0.028963412       0.44070127 -0.0419432803
    ## P_Child           0.008523377 -0.041475936       0.29931372 -0.0433279437
    ## P_Youth           0.027608101 -0.014130306       0.18755727 -0.0095854745
    ## P_Cook           -0.040256351 -0.026673876       0.30425340  0.0004968833
    ## P_DIY            -0.005309265 -0.025946174       0.22331539 -0.0089634125
    ## P_Art             0.357688817 -0.003500037       0.27248948 -0.0613754066
    ##                  Last_purchase      P_Child      P_Youth        P_Cook
    ## Choice              0.14143702  0.008523377  0.027608101 -0.0402563507
    ## Gender             -0.02896341 -0.041475936 -0.014130306 -0.0266738763
    ## Amount_purchased    0.44070127  0.299313719  0.187557270  0.3042533969
    ## Frequency          -0.04194328 -0.043327944 -0.009585474  0.0004968833
    ## Last_purchase       1.00000000  0.679133923  0.453258910  0.6725053933
    ## P_Child             0.67913392  1.000000000  0.174826719  0.2947065185
    ## P_Youth             0.45325891  0.174826719  1.000000000  0.1816566401
    ## P_Cook              0.67250539  0.294706519  0.181656640  1.0000000000
    ## P_DIY               0.55816739  0.253837077  0.188683456  0.2717251256
    ## P_Art               0.53433415  0.224512850  0.141751220  0.1916807611
    ##                         P_DIY        P_Art
    ## Choice           -0.005309265  0.357688817
    ## Gender           -0.025946174 -0.003500037
    ## Amount_purchased  0.223315392  0.272489483
    ## Frequency        -0.008963412 -0.061375407
    ## Last_purchase     0.558167395  0.534334145
    ## P_Child           0.253837077  0.224512850
    ## P_Youth           0.188683456  0.141751220
    ## P_Cook            0.271725126  0.191680761
    ## P_DIY             1.000000000  0.207791065
    ## P_Art             0.207791065  1.000000000

Drop `Last_purchase` because of its moderate-to-high correlation with
four other variables `P_Child`, `P_Cook`, `P_DIY`, `P_Art`.

Checking for correlation among remaining variables.

``` r
bbbctrain|>
  select(-First_purchase, -Last_purchase)|>
  cor()
```

    ##                        Choice       Gender Amount_purchased     Frequency
    ## Choice            1.000000000 -0.141558415       0.11815256 -0.2260181193
    ## Gender           -0.141558415  1.000000000      -0.03060700  0.0321704951
    ## Amount_purchased  0.118152563 -0.030607000       1.00000000  0.0136664846
    ## Frequency        -0.226018119  0.032170495       0.01366648  1.0000000000
    ## P_Child           0.008523377 -0.041475936       0.29931372 -0.0433279437
    ## P_Youth           0.027608101 -0.014130306       0.18755727 -0.0095854745
    ## P_Cook           -0.040256351 -0.026673876       0.30425340  0.0004968833
    ## P_DIY            -0.005309265 -0.025946174       0.22331539 -0.0089634125
    ## P_Art             0.357688817 -0.003500037       0.27248948 -0.0613754066
    ##                       P_Child      P_Youth        P_Cook        P_DIY
    ## Choice            0.008523377  0.027608101 -0.0402563507 -0.005309265
    ## Gender           -0.041475936 -0.014130306 -0.0266738763 -0.025946174
    ## Amount_purchased  0.299313719  0.187557270  0.3042533969  0.223315392
    ## Frequency        -0.043327944 -0.009585474  0.0004968833 -0.008963412
    ## P_Child           1.000000000  0.174826719  0.2947065185  0.253837077
    ## P_Youth           0.174826719  1.000000000  0.1816566401  0.188683456
    ## P_Cook            0.294706519  0.181656640  1.0000000000  0.271725126
    ## P_DIY             0.253837077  0.188683456  0.2717251256  1.000000000
    ## P_Art             0.224512850  0.141751220  0.1916807611  0.207791065
    ##                         P_Art
    ## Choice            0.357688817
    ## Gender           -0.003500037
    ## Amount_purchased  0.272489483
    ## Frequency        -0.061375407
    ## P_Child           0.224512850
    ## P_Youth           0.141751220
    ## P_Cook            0.191680761
    ## P_DIY             0.207791065
    ## P_Art             1.000000000

Correlation looks good across the remaining variables: `Choice`,
`Gender`, `Amount_purchased`, `Frequency`, `P_Child`, `P_Youth`,
`P_Cook`, `P_DIY`, `P_Art`

Initializing new objects for clean data.

``` r
bbbctrain_clean = bbbctrain|>
  select(-First_purchase, -Last_purchase)
```

``` r
bbbctest_clean = bbbctest|>
  select(-First_purchase, -Last_purchase)
```

Exploring data and plots

``` r
str(bbbctrain_clean)
```

    ## tibble [1,600 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ Choice          : num [1:1600] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Gender          : num [1:1600] 1 1 1 1 0 1 1 0 1 1 ...
    ##  $ Amount_purchased: num [1:1600] 113 418 336 180 320 268 198 280 393 138 ...
    ##  $ Frequency       : num [1:1600] 8 6 18 16 2 4 2 6 12 10 ...
    ##  $ P_Child         : num [1:1600] 0 0 2 2 0 0 2 0 3 2 ...
    ##  $ P_Youth         : num [1:1600] 1 2 0 0 0 0 3 2 0 3 ...
    ##  $ P_Cook          : num [1:1600] 0 3 1 0 0 0 2 0 3 0 ...
    ##  $ P_DIY           : num [1:1600] 0 2 1 1 1 0 1 0 0 0 ...
    ##  $ P_Art           : num [1:1600] 0 3 2 1 2 0 2 0 2 1 ...

``` r
par(mfrow = c(3 ,2))
hist(bbbctrain_clean$Frequency, main = "Frequency")
hist(bbbctrain_clean$P_Child, main = "P_Child")
hist(bbbctrain_clean$P_Youth, main = "P_Youth")
hist(bbbctrain_clean$P_Cook, main = "P_Cook")
hist(bbbctrain_clean$P_DIY, main = "P_DIY")
hist(bbbctrain_clean$P_Art, main = "P_Art")
```

![](CaseStudy2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
par(mfrow = c(1,2))
plot(bbbctrain_clean$Choice, main = 'Choice')
plot(bbbctrain_clean$Gender, main = 'Gender')
```

![](CaseStudy2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

setting formula to an object

``` r
form1 = Choice ~ .
```

Building the linear model with our cleaned training data.

``` r
lm1 = lm(form1, data = bbbctrain_clean)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = form1, data = bbbctrain_clean)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9501 -0.2518 -0.1273  0.1509  1.1211 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.3731865  0.0302933  12.319  < 2e-16 ***
    ## Gender           -0.1263728  0.0203683  -6.204 6.99e-10 ***
    ## Amount_purchased  0.0003688  0.0001123   3.283  0.00105 ** 
    ## Frequency        -0.0112345  0.0012344  -9.101  < 2e-16 ***
    ## P_Child          -0.0275983  0.0100284  -2.752  0.00599 ** 
    ## P_Youth          -0.0014841  0.0159946  -0.093  0.92609    
    ## P_Cook           -0.0428346  0.0102155  -4.193 2.90e-05 ***
    ## P_DIY            -0.0384262  0.0153017  -2.511  0.01213 *  
    ## P_Art             0.2183323  0.0140081  15.586  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3856 on 1591 degrees of freedom
    ## Multiple R-squared:  0.2114, Adjusted R-squared:  0.2075 
    ## F-statistic: 53.32 on 8 and 1591 DF,  p-value: < 2.2e-16

We need 2 variables, including the response, to be factors, so a linear
model would not be appropriate for this data set.

Getting data ready for a logistic regression model.

set factor variables for logistic

``` r
bbbctrain_clean$Choice = as.factor(bbbctrain_clean$Choice)
bbbctest_clean$Choice = as.factor(bbbctest$Choice)

bbbctrain_clean$Gender = as.factor(bbbctrain_clean$Gender)
bbbctest_clean$Gender = as.factor(bbbctest$Gender)
```

``` r
str(bbbctrain_clean)
```

    ## tibble [1,600 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ Choice          : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Gender          : Factor w/ 2 levels "0","1": 2 2 2 2 1 2 2 1 2 2 ...
    ##  $ Amount_purchased: num [1:1600] 113 418 336 180 320 268 198 280 393 138 ...
    ##  $ Frequency       : num [1:1600] 8 6 18 16 2 4 2 6 12 10 ...
    ##  $ P_Child         : num [1:1600] 0 0 2 2 0 0 2 0 3 2 ...
    ##  $ P_Youth         : num [1:1600] 1 2 0 0 0 0 3 2 0 3 ...
    ##  $ P_Cook          : num [1:1600] 0 3 1 0 0 0 2 0 3 0 ...
    ##  $ P_DIY           : num [1:1600] 0 2 1 1 1 0 1 0 0 0 ...
    ##  $ P_Art           : num [1:1600] 0 3 2 1 2 0 2 0 2 1 ...

building logistic model (glm)

``` r
glm1 = glm(form1, data = bbbctrain_clean, family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = form1, family = binomial, data = bbbctrain_clean)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -0.286380   0.202966  -1.411  0.15825    
    ## Gender1          -0.811948   0.134579  -6.033 1.61e-09 ***
    ## Amount_purchased  0.002406   0.000771   3.120  0.00181 ** 
    ## Frequency        -0.088625   0.010385  -8.534  < 2e-16 ***
    ## P_Child          -0.194796   0.072207  -2.698  0.00698 ** 
    ## P_Youth          -0.031928   0.109605  -0.291  0.77082    
    ## P_Cook           -0.292392   0.072998  -4.005 6.19e-05 ***
    ## P_DIY            -0.279282   0.108094  -2.584  0.00977 ** 
    ## P_Art             1.245842   0.099062  12.576  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1799.5  on 1599  degrees of freedom
    ## Residual deviance: 1445.0  on 1591  degrees of freedom
    ## AIC: 1463
    ## 
    ## Number of Fisher Scoring iterations: 5

Checking for multicolinearity using `vif()`

``` r
vif(glm1)
```

    ##           Gender Amount_purchased        Frequency          P_Child 
    ##         1.020217         1.213528         1.015899         1.215500 
    ##          P_Youth           P_Cook            P_DIY            P_Art 
    ##         1.081019         1.228798         1.179821         1.229491

No multicolinearity present, so we can look to improve the model. We
will use stepwise selection to find best variables. After finding best
variables, multicolinnearity will be checked again.

``` r
glm1_step = step(glm1, direction = "both")
```

    ## Start:  AIC=1462.98
    ## Choice ~ Gender + Amount_purchased + Frequency + P_Child + P_Youth + 
    ##     P_Cook + P_DIY + P_Art
    ## 
    ##                    Df Deviance    AIC
    ## - P_Youth           1   1445.1 1461.1
    ## <none>                  1445.0 1463.0
    ## - P_DIY             1   1451.9 1467.9
    ## - P_Child           1   1452.6 1468.6
    ## - Amount_purchased  1   1454.8 1470.8
    ## - P_Cook            1   1462.1 1478.1
    ## - Gender            1   1481.5 1497.5
    ## - Frequency         1   1534.5 1550.5
    ## - P_Art             1   1639.8 1655.8
    ## 
    ## Step:  AIC=1461.07
    ## Choice ~ Gender + Amount_purchased + Frequency + P_Child + P_Cook + 
    ##     P_DIY + P_Art
    ## 
    ##                    Df Deviance    AIC
    ## <none>                  1445.1 1461.1
    ## + P_Youth           1   1445.0 1463.0
    ## - P_DIY             1   1452.2 1466.2
    ## - P_Child           1   1452.9 1466.9
    ## - Amount_purchased  1   1454.8 1468.8
    ## - P_Cook            1   1462.5 1476.5
    ## - Gender            1   1481.6 1495.6
    ## - Frequency         1   1534.5 1548.5
    ## - P_Art             1   1640.1 1654.1

``` r
summary(glm1_step)
```

    ## 
    ## Call:
    ## glm(formula = Choice ~ Gender + Amount_purchased + Frequency + 
    ##     P_Child + P_Cook + P_DIY + P_Art, family = binomial, data = bbbctrain_clean)
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -0.2894506  0.2026211  -1.429  0.15314    
    ## Gender1          -0.8120440  0.1345723  -6.034  1.6e-09 ***
    ## Amount_purchased  0.0023859  0.0007678   3.108  0.00189 ** 
    ## Frequency        -0.0885491  0.0103772  -8.533  < 2e-16 ***
    ## P_Child          -0.1964495  0.0720116  -2.728  0.00637 ** 
    ## P_Cook           -0.2940503  0.0727520  -4.042  5.3e-05 ***
    ## P_DIY            -0.2823065  0.1076089  -2.623  0.00870 ** 
    ## P_Art             1.2441330  0.0988603  12.585  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1799.5  on 1599  degrees of freedom
    ## Residual deviance: 1445.1  on 1592  degrees of freedom
    ## AIC: 1461.1
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
vif(glm1_step)
```

    ##           Gender Amount_purchased        Frequency          P_Child 
    ##         1.020262         1.204294         1.015220         1.208836 
    ##           P_Cook            P_DIY            P_Art 
    ##         1.221915         1.169499         1.225022

The best logistic regression model includes the variables `Gender`,
`Amount_purchased`, `Frequency`, `P_Child`, `P_Cook`, `P_DIY`, and
`P_Art`. No multicolinearity was present.

Next we predict what the response variable will be for each observation.
We want to predict if each observation will be a 1 or 0 for the response
variable, `Choice`, based on a specific threshold, 0.204, that was found
with an optimal threshold function.

``` r
bbbctest_clean$PredProb = predict.glm(glm1_step, newdata = bbbctest_clean, type = "response")
bbbctest_clean$PredChoice = ifelse(bbbctest_clean$PredProb >= .204318, 1, 0)
caret::confusionMatrix(as.factor(bbbctest_clean$PredChoice),as.factor(bbbctest_clean$Choice),
                       positive = '1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1369   42
    ##          1  727  162
    ##                                          
    ##                Accuracy : 0.6657         
    ##                  95% CI : (0.646, 0.6849)
    ##     No Information Rate : 0.9113         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.1778         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.79412        
    ##             Specificity : 0.65315        
    ##          Pos Pred Value : 0.18223        
    ##          Neg Pred Value : 0.97023        
    ##              Prevalence : 0.08870        
    ##          Detection Rate : 0.07043        
    ##    Detection Prevalence : 0.38652        
    ##       Balanced Accuracy : 0.72363        
    ##                                          
    ##        'Positive' Class : 1              
    ## 

The predictions from our logistic regression model had the highest
sensitivity.

Finding the most optimal cutoff point.

``` r
proc_log = roc(bbbctest_clean$Choice, bbbctest_clean$PredProb)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(proc_log)
```

![](CaseStudy2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
coords(proc_log, "best", ret = "threshold")
```

    ##   threshold
    ## 1 0.2043183

Next we want to fit a support vector machine and to make predictions on
our response variable like we did with the logistic regression.

radial kernel support vector machine

``` r
form_svm = Choice ~ .
```

``` r
tuned = tune.svm(form_svm, data = bbbctrain_clean, gamma = seq(.01, .1, by = .01), cost = seq(.1, 1, by = .1))
```

``` r
mysvm = svm(formula = form_svm, data = bbbctrain_clean, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(mysvm)
```

    ## 
    ## Call:
    ## svm(formula = form_svm, data = bbbctrain_clean, gamma = tuned$best.parameters$gamma, 
    ##     cost = tuned$best.parameters$cost)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  0.8 
    ## 
    ## Number of Support Vectors:  812
    ## 
    ##  ( 368 444 )
    ## 
    ## 
    ## Number of Classes:  2 
    ## 
    ## Levels: 
    ##  0 1

``` r
svmpredict = predict(mysvm, bbbctest_clean, type = 'response', positive = 1)
table(pred = svmpredict, true = bbbctest_clean$Choice)
```

    ##     true
    ## pred    0    1
    ##    0 2034  157
    ##    1   62   47

Compare svm results to previous logistic model results (using optimally
calculated threshold). The radial kernel support vector machine has a
lower sensitivity, but higher accuracy.

Trying a sigmoid kernel svm

``` r
mysvm = svm(formula = form_svm, data = bbbctrain_clean, kernel = 'sigmoid', gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(mysvm)
```

    ## 
    ## Call:
    ## svm(formula = form_svm, data = bbbctrain_clean, kernel = "sigmoid", 
    ##     gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  sigmoid 
    ##        cost:  0.8 
    ##      coef.0:  0 
    ## 
    ## Number of Support Vectors:  575
    ## 
    ##  ( 286 289 )
    ## 
    ## 
    ## Number of Classes:  2 
    ## 
    ## Levels: 
    ##  0 1

``` r
svmpredict = predict(mysvm, bbbctest_clean, type = 'response', positive = 1)
table(pred = svmpredict, true = bbbctest_clean$Choice)
```

    ##     true
    ## pred    0    1
    ##    0 1762  117
    ##    1  334   87

The sigmoid kernel svm has a higher sensitivity, lower specificity, and
lower accuracy than the radial kernel.

The radial kernel svm has the highest accuracy and highes positive
predictive value out of the three models. The logistic regression model
has the highest sensitivity out of the three models.

Costs \$0.65 to mail brochure \$15 to send book to those who order
\$31.95 to company \$15 \* 0.45 is overhead

In our confusion matrix, there are 204 ‘yes’ responses. If BBBC were to
mail out to everyone on their list and send books to the 204 ‘yes’
choices, then their profit would be \$585.80.

Most sig ind is p art, target those people The sig f freq shows to not
target people who have a high freq value because they are less likely to
purchase since they have purchased multiple times recently.

The optimal cutoff threshold found by the function was, in fact, the
optimal cutoff for the business problem at hand. The logistic model with
cutoff of 0.204318 yielded a \$1074.56 profit. This was over an 80%
increase from the profit of \$585.80 from BBBC mailing to everyone on
their list. The significance of predictor variable P_Art indicates we
should target those people, and the significance of Frequency variable
lets us know not to target those people because they are less likely to
purchase since they have purchased multiple times recently.

The logistic model should be the one BBBC invests more into because the
linear model does not apply here and both radial and sigmoidal svm do
not make as much profit. Radial SVM makes \$327.35 losing the company
money in comparison to the mass mailing method, while sigmoidal SVM
makes \$632.20, an increase from the mass mailing method but
significantly less than the logistic model.
