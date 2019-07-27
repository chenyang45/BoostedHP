# Boosted Hodrick-Prescott Filter

This is an R package for Peter Phillips and Zhentao Shi (2019): ["Boosting the Hodrick-Prescott Filter"](https://arxiv.org/abs/1905.00175). The method is implemented by the function `BoostedHP()`. 

version : 0.0.5 (2019-07-22)




![image](https://github.com/chenyang45/BoostedHP/blob/master/ADF_bHP_ani.gif)


#### Installation

A preliminary R package can be installed by running in `R`
```
install.packages("devtools")
devtools::install_github("chenyang45/BoostedHP/BoostedHP")
library("BoostedHP")
```

# Important

To make sure robust installation, delete your package "curl" document in your R lib for updated install during the install_github() process.



Note: If you have problem while installing the package with the error message :
```
Error in read.dcf(path) : 
  Found continuation line starting '    tseries, ...' at begin of record.
```
The problem may comes from your new version of package "devtools" (2.0.1), just install the old version of package "devtools" (eg: 1.13.6) and the problem will be solved.
We provide the whole files of old version package "devtools"(version 1.13.6) in this repo, feel free to download and install it manually. 

Before installation, we also suggest you to install package "tseries" and "exmp" first cause sometimes the Internet speed is such low and when you install the three package at the same the procesure may be fail.

The package is in progress. We are learning how to make an R package for the first time.
