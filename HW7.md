糖尿病的預測模型
================

資料前處理
----------

### 資料讀取

資料來源為UCI Machine Learning Repository。

資料記載可能的因素來預測女性是否為糖尿病患者，共有八個參數。

分類結果為二元分類，非糖尿病患者(neg)&糖尿病患者(pos) 。

``` r
#install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes) 
```

    ## 'data.frame':    768 obs. of  9 variables:
    ##  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
    ##  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
    ##  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
    ##  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
    ##  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
    ##  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
    ##  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
    ##  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
    ##  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...

### 留下無缺值的資料

``` r
PimaIndiansDiabetesC<-
    PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),] 
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))
```

    ## [1] 768 768

### 將資料隨機分為訓練組與測試組

隨機將2/3的資料分到訓練組（Test==F）， 剩下1/3為測試組（Test==T）將有糖尿病症狀的人放在level1

``` r
PimaIndiansDiabetesC$Test<-F 
PimaIndiansDiabetesC[sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),]$Test<-T
PimaIndiansDiabetesC$diabetes<-factor(PimaIndiansDiabetesC$diabetes,levels=c("pos","neg"))
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T))
```

    ## [1] 512 256

訓練組案例數為512， 測試組案例數為256

預測模型建立
------------

### 模型建立

變數相當多，多為連續變項，輸出為二元的類別變項， 所以選擇邏輯迴歸演算法建立模型，使用雙向逐步選擇最佳參數組合。

``` r
fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")

#install.packages("MASS")

library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```

    ##                 Estimate  Std. Error   z value     Pr(>|z|)
    ## (Intercept)  8.559634560 0.891658842  9.599674 8.019736e-22
    ## pregnant    -0.137990737 0.034051734 -4.052385 5.069814e-05
    ## glucose     -0.034743365 0.004368585 -7.953002 1.820452e-15
    ## pressure     0.013877020 0.006379866  2.175127 2.962059e-02
    ## insulin      0.001856451 0.001120939  1.656157 9.769012e-02
    ## mass        -0.110128500 0.019061442 -5.777554 7.579461e-09
    ## pedigree    -0.843649200 0.368128669 -2.291724 2.192159e-02

### 模型說明

上述參數知，以邏輯迴歸建立模型預測是否為糖尿病患者，經最佳化後， 模型使用參數為pregnant, glucose, pressure, insulin, mass, pedigree， 共7個參數， 各參數代表每一個可能的因素

預測模型驗證
------------

``` r
DiabPred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
DiabAns<-ifelse(DiabPred<0.5,"pos","neg") 
DiabAns<-factor(DiabAns,levels = c("pos","neg"))
#install.packages("caret")

library(caret)
round(sensitivity(DiabAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes),digit = 2)
```

    ## [1] 0.59

``` r
round(specificity(DiabAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes),digit = 2)
```

    ## [1] 0.88

``` r
round(posPredValue(DiabAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes),digit = 2)
```

    ## [1] 0.71

``` r
round(negPredValue(DiabAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes),digit = 2)
```

    ## [1] 0.82

從不同女性得到的相關資料，經過邏輯回歸模型預測是否為糖尿病患者，可得：

-   1.敏感度 59.04%
-   2.特異性 88.44%
-   3.預測率
-   陽性預測率 71.01%
-   陰性預測率 81.82% 我們為了觀看
