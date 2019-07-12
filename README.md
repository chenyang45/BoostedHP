# Boosted Hodrick-Prescott Filter

This is an R package for Peter Phillips and Zhentao Shi (2019): ["Boosting the Hodrick-Prescott Filter"](https://arxiv.org/abs/1905.00175). The method is implemented by the function `BoostedHP()`. 

version : 0.0.3



#### Installation

A preliminary R package can be installed by running in `R`
```
install.packages("devtools")
devtools::install_github("chenyang45/BoostedHP/BoostedHP")
library("BoostedHP")
```
Note: If you have problem while installing the package with the error message :
```
Error in read.dcf(path) : 
  Found continuation line starting '    tseries, ...' at begin of record.
```
The problem comes from your new version of package "devtools" (2.0.1), just install the old version of package "devtools" (eg: 1.13.6) and the problem will be solved.
We provide the whole files of old version package "devtools"(version 1.13.6) in this repo, feel free to download and install it manually. 

Before installation, we also suggest you to install package "tseries" and "exmp" first cause sometimes the Internet speed is such low and when you install the three package at the same the procesure may be fail.

The package is in progress.

#### Example
```
data("IRE") # Ireland Annual GDP example in the paper, which is saved in the package.

lam = 100 # tuning parameter for the annual data

# raw HP filter
bx_HP = BoostedHP(IRE, lambda = lam, iter= FALSE)

# by BIC
bx_BIC = BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")

# by ADF
bx_ADF = BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf", sig_p = 0.050)

# summarize the outcome
outcome = cbind(IRE, bx_HP$trend, bx_BIC$trend, bx_ADF$trend) 
matplot(  outcome, type = "l", ylab = "", lwd = rep(2,4)  )
```

